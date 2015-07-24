;;; This is ASDF 3.1.5
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
   #:upgrading-p #:when-upgrading #:upgrade-asdf #:asdf-upgrade-error #:defparameter*
   #:*post-upgrade-cleanup-hook* #:*post-upgrade-restart-hook* #:cleanup-upgraded-asdf
   ;; There will be no symbol left behind!
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
  ;; Important: define *p-a-v* /before/ *a-v* so that it initializes correctly.
  (defvar *previous-asdf-versions* (if-let (previous (asdf-version)) (list previous)))
  (defvar *asdf-version* nil)
  ;; We need to clear systems from versions yet older than the below:
  (defparameter *oldest-forward-compatible-asdf-version* "2.33") ;; 2.32.13 renames a slot in component.
  (defvar *verbose-out* nil)
  (defun asdf-message (format-string &rest format-args)
    (when *verbose-out* (apply 'format *verbose-out* format-string format-args)))
  (defvar *post-upgrade-cleanup-hook* ())
  (defvar *post-upgrade-restart-hook* ())
  (defun upgrading-p (&optional (oldest-compatible-version *oldest-forward-compatible-asdf-version*))
    (and *previous-asdf-versions*
         (version< (first *previous-asdf-versions*) oldest-compatible-version)))
  (defmacro defparameter* (var value &optional docstring (version *oldest-forward-compatible-asdf-version*))
    (let* ((name (string-trim "*" var))
           (valfun (intern (format nil "%~A-~A-~A" :compute name :value))))
      `(progn
         (defun ,valfun () ,value)
         (defvar ,var (,valfun) ,@(ensure-list docstring))
         (when (upgrading-p ,version)
           (setf ,var (,valfun))))))
  (defmacro when-upgrading ((&key (version *oldest-forward-compatible-asdf-version*)
                               (upgrading-p `(upgrading-p ,version)) when) &body body)
    "A wrapper macro for code that should only be run when upgrading a
previously-loaded version of ASDF."
    `(with-upgradability ()
       (when (and ,upgrading-p ,@(when when `(,when)))
         (handler-bind ((style-warning #'muffle-warning))
           (eval '(progn ,@body))))))
  (let* (;; For bug reporting sanity, please always bump this version when you modify this file.
         ;; Please also modify asdf.asd to reflect this change. make bump-version v=3.4.5.67.8
         ;; can help you do these changes in synch (look at the source for documentation).
         ;; Relying on its automation, the version is now redundantly present on top of asdf.lisp.
         ;; "3.4" would be the general branch for major version 3, minor version 4.
         ;; "3.4.5" would be an official release in the 3.4 branch.
         ;; "3.4.5.67" would be a development version in the official branch, on top of 3.4.5.
         ;; "3.4.5.0.8" would be your eighth local modification of official release 3.4.5
         ;; "3.4.5.67.8" would be your eighth local modification of development version 3.4.5.67
         (asdf-version "3.1.5")
         (existing-version (asdf-version)))
    (setf *asdf-version* asdf-version)
    (when (and existing-version (not (equal asdf-version existing-version)))
      (push existing-version *previous-asdf-versions*)
      (when (or *verbose-out* *load-verbose*)
        (format (or *verbose-out* *trace-output*)
                (compatfmt "~&~@<; ~@;Upgrading ASDF ~@[from version ~A ~]to version ~A~@:>~%")
                existing-version asdf-version)))))

(when-upgrading ()
  (let ((redefined-functions ;; gf signature and/or semantics changed incompatibly. Oops.
          ;; NB: it's too late to do anything about functions in UIOP!
          ;; If you introduce some critically incompatibility there, you must change name.
          '(#:component-relative-pathname #:component-parent-pathname ;; component
            #:source-file-type
            #:find-system #:system-source-file #:system-relative-pathname ;; system
            #:find-component ;; find-component
            #:explain #:perform #:perform-with-restarts #:input-files #:output-files ;; action
            #:component-depends-on #:operation-done-p #:component-depends-on
            #:traverse ;; backward-interface
            #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies ;; plan
            #:operate  ;; operate
            #:parse-component-form ;; defsystem
            #:apply-output-translations ;; output-translations
            #:process-output-translations-directive
            #:inherit-source-registry #:process-source-registry ;; source-registry
            #:process-source-registry-directive
            #:trivial-system-p)) ;; bundle
        (redefined-classes
          ;; redefining the classes causes interim circularities
          ;; with the old ASDF during upgrade, and many implementations bork
          '((#:compile-concatenated-source-op (#:operation) ()))))
    (loop :for name :in redefined-functions
          :for sym = (find-symbol* name :asdf nil) :do
            (when sym
              ;; On CLISP we seem to be unable to fmakunbound and define a function in the same fasl. Sigh.
              #-clisp (fmakunbound sym)))
    (labels ((asym (x) (multiple-value-bind (s p) (if (consp x) (values (car x) (cadr x)) (values x :asdf))
                         (find-symbol* s p nil)))
             (asyms (l) (mapcar #'asym l)))
      (loop* :for (name superclasses slots) :in redefined-classes
             :for sym = (find-symbol* name :asdf nil)
             :when (and sym (find-class sym))
             :do (eval `(defclass ,sym ,(asyms superclasses) ,(asyms slots)))))))


;;; Self-upgrade functions

(with-upgradability ()
  (defun asdf-upgrade-error ()
    ;; Important notice for whom it concerns. The crux of the matter is that
    ;; TRAVERSE can be completely refactored, and so after the find-system returns, it's too late.
    (error "When a system transitively depends on ASDF, it must :defsystem-depends-on (:asdf)~%~
          Otherwise, when you upgrade from ASDF 2, you must do it before you operate on any system.~%"))

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

  (register-hook-function '*post-upgrade-cleanup-hook* 'upgrade-configuration))

;;;; -------------------------------------------------------------------------
;;;; Components

(uiop/package:define-package :asdf/component
  (:recycle :asdf/component :asdf/defsystem :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:component #:component-find-path
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
   #:system-definition-error ;; top level, moved here because this is the earliest place for it.
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
    (:documentation "Find the top-level system containing COMPONENT"))
  (defgeneric component-pathname (component)
    (:documentation "Extracts the pathname applicable for a particular component."))
  (defgeneric (component-relative-pathname) (component)
    (:documentation "Returns a pathname for the component argument intended to be
interpreted relative to the pathname of that component's parent.
Despite the function's name, the return value may be an absolute
pathname, because an absolute pathname may be interpreted relative to
another pathname in a degenerate way."))
  (defgeneric component-external-format (component))
  (defgeneric component-encoding (component))
  (defgeneric version-satisfies (component version))
  (defgeneric component-version (component))
  (defgeneric (setf component-version) (new-version component))
  (defgeneric component-parent (component))
  (defmethod component-parent ((component null)) nil)

  ;; Backward compatible way of computing the FILE-TYPE of a component.
  ;; TODO: find users, have them stop using that, remove it for ASDF4.
  (defgeneric (source-file-type) (component system))

  (define-condition system-definition-error (error) ()
    ;; [this use of :report should be redundant, but unfortunately it's not.
    ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
    ;; over print-object; this is always conditions::%print-condition for
    ;; condition objects, which in turn does inheritance of :report options at
    ;; run-time.  fortunately, inheritance means we only need this kludge here in
    ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
    #+cmu (:report print-object))

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
     ;; methods defined using the "inline" style inside a defsystem form:
     ;; need to store them somewhere so we can delete them when the system
     ;; is re-evaluated.
     (inline-methods :accessor component-inline-methods :initform nil) ;; OBSOLETE! DELETE THIS IF NO ONE USES.
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
      :initarg :build-operation :initform nil :reader component-build-operation)))

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
    (:documentation "A CHILD-COMPONENT is a component that may be part of
a PARENT-COMPONENT."))

  (defclass file-component (child-component)
    ((type :accessor file-type :initarg :type))) ; no default
  (defclass source-file (file-component)
    ((type :accessor source-file-explicit-type ;; backward-compatibility
           :initform nil))) ;; NB: many systems have come to rely on this default.
  (defclass c-source-file (source-file)
    ((type :initform "c")))
  (defclass java-source-file (source-file)
    ((type :initform "java")))
  (defclass static-file (source-file)
    ((type :initform nil)))
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
  (:documentation "A PARENT-COMPONENT is a component that may have
children.")))

(with-upgradability ()
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
    (#+clisp (components)))) ;; backward compatibility during upgrade only


;;;; component pathnames
(with-upgradability ()
  (defgeneric* (component-parent-pathname) (component))
  (defmethod component-parent-pathname (component)
    (component-pathname (component-parent component)))

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
  (defgeneric around-compile-hook (component))
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
    (while-collecting (c)
      (labels ((recurse (x)
                 (when (if-let (it (component-if-feature x)) (featurep it) t)
                   (when (typep x type)
                     (c x))
                   (when (typep x 'parent-component)
                     (map () #'recurse (component-children x))))))
        (recurse component)))))

;;;; -------------------------------------------------------------------------
;;;; Systems

(uiop/package:define-package :asdf/system
  (:recycle :asdf :asdf/system)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/component)
  (:export
   #:system #:proto-system
   #:system-source-file #:system-source-directory #:system-relative-pathname
   #:reset-system
   #:system-description #:system-long-description
   #:system-author #:system-maintainer #:system-licence #:system-license
   #:system-defsystem-depends-on #:system-depends-on #:system-weakly-depends-on
   #:component-build-pathname #:build-pathname
   #:component-entry-point #:entry-point
   #:homepage #:system-homepage
   #:bug-tracker #:system-bug-tracker
   #:mailto #:system-mailto
   #:long-name #:system-long-name
   #:source-control #:system-source-control
   #:find-system #:builtin-system-p)) ;; forward-reference, defined in find-system
(in-package :asdf/system)

(with-upgradability ()
  (defgeneric* (find-system) (system &optional error-p))
  (defgeneric* (system-source-file :supersede #-clisp t #+clisp nil) (system)
    (:documentation "Return the source file in which system is defined."))
  (defgeneric component-build-pathname (component))

  (defgeneric component-entry-point (component))
  (defmethod component-entry-point ((c component))
    nil))


;;;; The system class

(with-upgradability ()
  (defclass proto-system () ; slots to keep when resetting a system
    ;; To preserve identity for all objects, we'd need keep the components slots
    ;; but also to modify parse-component-form to reset the recycled objects.
    ((name) (source-file) #|(children) (children-by-names)|#))

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
     (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on
                           :initform nil)
     ;; these two are specially set in parse-component-form, so have no :INITARGs.
     (depends-on :reader system-depends-on :initform nil)
     (weakly-depends-on :reader system-weakly-depends-on :initform nil)))

  (defun reset-system (system &rest keys &key &allow-other-keys)
    (change-class (change-class system 'proto-system) 'system)
    (apply 'reinitialize-instance system keys)))


;;;; Pathnames

(with-upgradability ()
  (defmethod system-source-file ((system-name string))
    (system-source-file (find-system system-name)))
  (defmethod system-source-file ((system-name symbol))
    (system-source-file (find-system system-name)))

  (defun system-source-directory (system-designator)
    "Return a pathname object corresponding to the directory
in which the system specification (.asd file) is located."
    (pathname-directory-pathname (system-source-file system-designator)))

  (defun (system-relative-pathname) (system name &key type)
    (subpathname (system-source-directory system) name :type type))

  (defmethod component-pathname ((system system))
    (let ((pathname (or (call-next-method) (system-source-directory system))))
      (unless (and (slot-boundp system 'relative-pathname) ;; backward-compatibility with ASDF1-age
                   (slot-value system 'relative-pathname)) ;; systems that directly access this slot.
        (setf (slot-value system 'relative-pathname) pathname))
      pathname))

  (defmethod component-relative-pathname ((system system))
    (parse-unix-namestring
     (and (slot-boundp system 'relative-pathname)
          (slot-value system 'relative-pathname))
     :want-relative t
     :type :directory
     :ensure-absolute t
     :defaults (system-source-directory system)))

  (defmethod component-parent-pathname ((system system))
    (system-source-directory system))

  (defmethod component-build-pathname ((c component))
    nil))

;;;; -------------------------------------------------------------------------
;;;; Stamp cache

(uiop/package:define-package :asdf/cache
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export #:get-file-stamp #:compute-file-stamp #:register-file-stamp
           #:set-asdf-cache-entry #:unset-asdf-cache-entry #:consult-asdf-cache
           #:do-asdf-cache #:normalize-namestring
           #:call-with-asdf-cache #:with-asdf-cache #:*asdf-cache*
           #:clear-configuration-and-retry #:retry))
(in-package :asdf/cache)

;;; This stamp cache is useful for:
;; * consistency of stamps used within a single run
;; * fewer accesses to the filesystem
;; * the ability to test with fake timestamps, without touching files

(with-upgradability ()
  (defvar *asdf-cache* nil)

  (defun set-asdf-cache-entry (key value-list)
    (apply 'values
           (if *asdf-cache*
               (setf (gethash key *asdf-cache*) value-list)
               value-list)))

  (defun unset-asdf-cache-entry (key)
    (when *asdf-cache*
      (remhash key *asdf-cache*)))

  (defun consult-asdf-cache (key &optional thunk)
    (if *asdf-cache*
        (multiple-value-bind (results foundp) (gethash key *asdf-cache*)
          (if foundp
              (apply 'values results)
              (set-asdf-cache-entry key (multiple-value-list (call-function thunk)))))
        (call-function thunk)))

  (defmacro do-asdf-cache (key &body body)
    `(consult-asdf-cache ,key #'(lambda () ,@body)))

  (defun call-with-asdf-cache (thunk &key override key)
    (let ((fun (if key #'(lambda () (consult-asdf-cache key thunk)) thunk)))
      (if (and *asdf-cache* (not override))
          (funcall fun)
          (loop
            (restart-case
                (let ((*asdf-cache* (make-hash-table :test 'equal)))
                  (return (funcall fun)))
              (retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation.~@:>"))))
              (clear-configuration-and-retry ()
                :report (lambda (s)
                          (format s (compatfmt "~@<Retry ASDF operation after resetting the configuration.~@:>")))
                (clear-configuration)))))))

  (defmacro with-asdf-cache ((&key key override) &body body)
    `(call-with-asdf-cache #'(lambda () ,@body) :override ,override :key ,key))

  (defun normalize-namestring (pathname)
    (let ((resolved (resolve-symlinks*
                     (ensure-absolute-pathname
                      (physicalize-pathname pathname)
                      'get-pathname-defaults))))
      (with-pathname-defaults () (namestring resolved))))

  (defun compute-file-stamp (normalized-namestring)
    (with-pathname-defaults ()
      (safe-file-write-date normalized-namestring)))

  (defun register-file-stamp (file &optional (stamp nil stampp))
    (let* ((namestring (normalize-namestring file))
           (stamp (if stampp stamp (compute-file-stamp namestring))))
      (set-asdf-cache-entry `(get-file-stamp ,namestring) (list stamp))))

  (defun get-file-stamp (file)
    (when file
      (let ((namestring (normalize-namestring file)))
        (do-asdf-cache `(get-file-stamp ,namestring) (compute-file-stamp namestring))))))

;;;; -------------------------------------------------------------------------
;;;; Finding systems

(uiop/package:define-package :asdf/find-system
  (:recycle :asdf/find-system :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
    :asdf/cache :asdf/component :asdf/system)
  (:export
   #:remove-entry-from-registry #:coerce-entry-to-directory
   #:coerce-name #:primary-system-name #:coerce-filename
   #:find-system #:locate-system #:load-asd
   #:system-registered-p #:register-system #:registered-systems #:clear-system #:map-systems
   #:missing-component #:missing-requires #:missing-parent
   #:formatted-system-definition-error #:format-control #:format-arguments #:sysdef-error
   #:load-system-definition-error #:error-name #:error-pathname #:error-condition
   #:*system-definition-search-functions* #:search-for-system-definition
   #:*central-registry* #:probe-asd #:sysdef-central-registry-search
   #:find-system-if-being-defined
   #:contrib-sysdef-search #:sysdef-find-asdf ;; backward compatibility symbols, functions removed
   #:sysdef-preloaded-system-search #:register-preloaded-system #:*preloaded-systems*
   #:sysdef-immutable-system-search #:register-immutable-system #:*immutable-systems*
   #:*defined-systems* #:clear-defined-systems
   ;; defined in source-registry, but specially mentioned here:
   #:initialize-source-registry #:sysdef-source-registry-search))
(in-package :asdf/find-system)

(with-upgradability ()
  (declaim (ftype (function (&optional t) t) initialize-source-registry)) ; forward reference

  (define-condition missing-component (system-definition-error)
    ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
     (parent :initform nil :reader missing-parent :initarg :parent)))

  (define-condition formatted-system-definition-error (system-definition-error)
    ((format-control :initarg :format-control :reader format-control)
     (format-arguments :initarg :format-arguments :reader format-arguments))
    (:report (lambda (c s)
               (apply 'format s (format-control c) (format-arguments c)))))

  (define-condition load-system-definition-error (system-definition-error)
    ((name :initarg :name :reader error-name)
     (pathname :initarg :pathname :reader error-pathname)
     (condition :initarg :condition :reader error-condition))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Error while trying to load definition for system ~A from pathname ~A: ~3i~_~A~@:>")
                       (error-name c) (error-pathname c) (error-condition c)))))

  (defun sysdef-error (format &rest arguments)
    (error 'formatted-system-definition-error :format-control
           format :format-arguments arguments))

  (defun coerce-name (name)
    (typecase name
      (component (component-name name))
      (symbol (string-downcase (symbol-name name)))
      (string name)
      (t (sysdef-error (compatfmt "~@<Invalid component designator: ~3i~_~A~@:>") name))))

  (defun primary-system-name (name)
    ;; When a system name has slashes, the file with defsystem is named by
    ;; the first of the slash-separated components.
    (first (split-string (coerce-name name) :separator "/")))

  (defun coerce-filename (name)
    (frob-substrings (coerce-name name) '("/" ":" "\\") "--"))

  (defvar *defined-systems* (make-hash-table :test 'equal)
    "This is a hash table whose keys are strings, being the
names of the systems, and whose values are pairs, the first
element of which is a universal-time indicating when the
system definition was last updated, and the second element
of which is a system object.")

  (defun system-registered-p (name)
    (gethash (coerce-name name) *defined-systems*))

  (defun registered-systems ()
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :collect (coerce-name (cdr registered))))

  (defun register-system (system)
    (check-type system system)
    (let ((name (component-name system)))
      (check-type name string)
      (asdf-message (compatfmt "~&~@<; ~@;Registering ~3i~_~A~@:>~%") system)
      (unless (eq system (cdr (gethash name *defined-systems*)))
        (setf (gethash name *defined-systems*)
              (cons (if-let (file (ignore-errors (system-source-file system)))
                      (get-file-stamp file))
                    system)))))

  (defvar *preloaded-systems* (make-hash-table :test 'equal))

  (defun make-preloaded-system (name keys)
    (apply 'make-instance (getf keys :class 'system)
           :name name :source-file (getf keys :source-file)
           (remove-plist-keys '(:class :name :source-file) keys)))

  (defun sysdef-preloaded-system-search (requested)
    (let ((name (coerce-name requested)))
      (multiple-value-bind (keys foundp) (gethash name *preloaded-systems*)
        (when foundp
          (make-preloaded-system name keys)))))

  (defun register-preloaded-system (system-name &rest keys)
    (setf (gethash (coerce-name system-name) *preloaded-systems*) keys))

  (dolist (s '("asdf" "uiop" "asdf-driver" "asdf-defsystem" "asdf-package-system"))
    ;; don't bother with these, no one relies on them: "asdf-utils" "asdf-bundle"
    (register-preloaded-system s :version *asdf-version*))

  (defvar *immutable-systems* nil
    "An hash-set (equal hash-table mapping keys to T) of systems that are immutable,
i.e. already loaded in memory and not to be refreshed from the filesystem.
They will be treated specially by find-system, and passed as :force-not argument to make-plan.

If you deliver an image with many systems precompiled, *and* do not want to check the filesystem
for them every time a user loads an extension, what more risk a problematic upgrade or catastrophic
downgrade, before you dump an image, use:
   (setf asdf::*immutable-systems* (uiop:list-to-hash-set (asdf:already-loaded-systems)))")

  (defun sysdef-immutable-system-search (requested)
    (let ((name (coerce-name requested)))
      (when (and *immutable-systems* (gethash name *immutable-systems*))
        (or (cdr (system-registered-p requested))
            (sysdef-preloaded-system-search name)
            (error 'formatted-system-definition-error
                   :format-control "Requested system ~A is in the *immutable-systems* set, ~
but not loaded in memory"
                   :format-arguments (list name))))))

  (defun register-immutable-system (system-name &key (version t))
    (let* ((system-name (coerce-name system-name))
           (registered-system (cdr (system-registered-p system-name)))
           (default-version? (eql version t))
           (version (cond ((and default-version? registered-system)
                           (component-version registered-system))
                          (default-version? nil)
                          (t version))))
      (unless registered-system
        (register-system (make-preloaded-system system-name (list :version version))))
      (register-preloaded-system system-name :version version)
      (unless *immutable-systems*
        (setf *immutable-systems* (list-to-hash-set nil)))
      (setf (gethash (coerce-name system-name) *immutable-systems*) t)))

  (defun clear-system (system)
    "Clear the entry for a SYSTEM in the database of systems previously loaded,
unless the system appears in the table of *IMMUTABLE-SYSTEMS*.
Note that this does NOT in any way cause the code of the system to be unloaded.
Returns T if cleared or already cleared,
NIL if not cleared because the system was found to be immutable."
    ;; There is no "unload" operation in Common Lisp, and
    ;; a general such operation cannot be portably written,
    ;; considering how much CL relies on side-effects to global data structures.
    (let ((name (coerce-name system)))
      (unless (and *immutable-systems* (gethash name *immutable-systems*))
        (remhash (coerce-name name) *defined-systems*)
        (unset-asdf-cache-entry `(locate-system ,name))
        (unset-asdf-cache-entry `(find-system ,name))
        t)))

  (defun clear-defined-systems ()
    ;; Invalidate all systems but ASDF itself, if registered.
    (loop :for name :being :the :hash-keys :of *defined-systems*
          :unless (equal name "asdf") :do (clear-system name)))

  (register-hook-function '*post-upgrade-cleanup-hook* 'clear-defined-systems nil)

  (defun map-systems (fn)
    "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
    (loop :for registered :being :the :hash-values :of *defined-systems*
          :do (funcall fn (cdr registered)))))

;;; for the sake of keeping things reasonably neat, we adopt a
;;; convention that functions in this list are prefixed SYSDEF-
(with-upgradability ()
  (defvar *system-definition-search-functions* '())

  (defun cleanup-system-definition-search-functions ()
    (setf *system-definition-search-functions*
          (append
           ;; Remove known-incompatible sysdef functions from old versions of asdf.
           (remove-if #'(lambda (x) (member x '(contrib-sysdef-search sysdef-find-asdf sysdef-preloaded-system-search)))
                      *system-definition-search-functions*)
           ;; Tuck our defaults at the end of the list if they were absent.
           ;; This is imperfect, in case they were removed on purpose,
           ;; but then it will be the responsibility of whoever does that
           ;; to upgrade asdf before he does such a thing rather than after.
           (remove-if #'(lambda (x) (member x *system-definition-search-functions*))
                      '(sysdef-central-registry-search
                        sysdef-source-registry-search)))))
  (cleanup-system-definition-search-functions)

  (defun search-for-system-definition (system)
    (let ((name (coerce-name system)))
      (flet ((try (f) (if-let ((x (funcall f name))) (return-from search-for-system-definition x))))
        (try 'find-system-if-being-defined)
        (try 'sysdef-immutable-system-search)
        (map () #'try *system-definition-search-functions*)
        (try 'sysdef-preloaded-system-search))))

  (defvar *central-registry* nil
    "A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This is for backward compatibility.
Going forward, we recommend new users should be using the source-registry.
")

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
                            (subseq *central-registry* (1+ position))))))))))

  (defmethod find-system ((name null) &optional (error-p t))
    (when error-p
      (sysdef-error (compatfmt "~@<NIL is not a valid system name~@:>"))))

  (defmethod find-system (name &optional (error-p t))
    (find-system (coerce-name name) error-p))

  (defun find-system-if-being-defined (name)
    ;; notable side effect: mark the system as being defined, to avoid infinite loops
    (first (gethash `(find-system ,(coerce-name name)) *asdf-cache*)))

  (defun load-asd (pathname
                   &key name (external-format (encoding-external-format (detect-encoding pathname)))
                   &aux (readtable *readtable*) (print-pprint-dispatch *print-pprint-dispatch*))
    ;; Tries to load system definition with canonical NAME from PATHNAME.
    (with-asdf-cache ()
      (with-standard-io-syntax
        (let ((*package* (find-package :asdf-user))
              ;; Note that our backward-compatible *readtable* is
              ;; a global readtable that gets globally side-effected. Ouch.
              ;; Same for the *print-pprint-dispatch* table.
              ;; We should do something about that for ASDF3 if possible, or else ASDF4.
              (*readtable* readtable)
              (*print-pprint-dispatch* print-pprint-dispatch)
              (*print-readably* nil)
              (*default-pathname-defaults*
                ;; resolve logical-pathnames so they won't wreak havoc in parsing namestrings.
                (pathname-directory-pathname (physicalize-pathname pathname))))
          (handler-bind
              ((error #'(lambda (condition)
                          (error 'load-system-definition-error
                                 :name name :pathname pathname
                                 :condition condition))))
            (asdf-message (compatfmt "~&~@<; ~@;Loading system definition~@[ for ~A~] from ~A~@:>~%")
                          name pathname)
            (load* pathname :external-format external-format))))))

  (defvar *old-asdf-systems* (make-hash-table :test 'equal))

  (defun check-not-old-asdf-system (name pathname)
    (or (not (equal name "asdf"))
        (null pathname)
        (let* ((version-pathname (subpathname pathname "version.lisp-expr"))
               (version (and (probe-file* version-pathname :truename nil)
                             (read-file-form version-pathname)))
               (old-version (asdf-version)))
          (cond
            ((version< old-version version) t) ;; newer version: good!
            ((equal old-version version) nil) ;; same version: don't load, but don't warn
            (t ;; old version: bad
             (ensure-gethash
              (list (namestring pathname) version) *old-asdf-systems*
              #'(lambda ()
                 (let ((old-pathname
                         (if-let (pair (system-registered-p "asdf"))
                           (system-source-file (cdr pair)))))
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
                         old-version old-pathname version pathname))))
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
    (let* ((name (coerce-name name))
           (in-memory (system-registered-p name)) ; load from disk if absent or newer on disk
           (previous (cdr in-memory))
           (previous (and (typep previous 'system) previous))
           (previous-time (car in-memory))
           (found (search-for-system-definition name))
           (found-system (and (typep found 'system) found))
           (pathname (ensure-pathname
                      (or (and (typep found '(or pathname string)) (pathname found))
                          (and found-system (system-source-file found-system))
                          (and previous (system-source-file previous)))
                      :want-absolute t :resolve-symlinks *resolve-symlinks*))
           (foundp (and (or found-system pathname previous) t)))
      (check-type found (or null pathname system))
      (unless (check-not-old-asdf-system name pathname)
        (cond
          (previous (setf found nil pathname nil))
          (t
           (setf found (sysdef-preloaded-system-search "asdf"))
           (assert (typep found 'system))
           (setf found-system found pathname nil))))
      (values foundp found-system pathname previous previous-time)))

  (defmethod find-system ((name string) &optional (error-p t))
    (with-asdf-cache (:key `(find-system ,name))
      (let ((primary-name (primary-system-name name)))
        (unless (equal name primary-name)
          (find-system primary-name nil)))
      (or (and *immutable-systems* (gethash name *immutable-systems*)
               (or (cdr (system-registered-p name))
                   (sysdef-preloaded-system-search name)))
          (multiple-value-bind (foundp found-system pathname previous previous-time)
              (locate-system name)
            (assert (eq foundp (and (or found-system pathname previous) t)))
            (let ((previous-pathname (and previous (system-source-file previous)))
                  (system (or previous found-system)))
              (when (and found-system (not previous))
                (register-system found-system))
              (when (and system pathname)
                (setf (system-source-file system) pathname))
              (when (and pathname
                         (let ((stamp (get-file-stamp pathname)))
                           (and stamp
                                (not (and previous
                                          (or (pathname-equal pathname previous-pathname)
                                              (and pathname previous-pathname
                                                   (pathname-equal
                                                    (physicalize-pathname pathname)
                                                    (physicalize-pathname previous-pathname))))
                                          (stamp<= stamp previous-time))))))
                ;; only load when it's a pathname that is different or has newer content, and not an old asdf
                (load-asd pathname :name name)))
            (let ((in-memory (system-registered-p name))) ; try again after loading from disk if needed
              (cond
                (in-memory
                 (when pathname
                   (setf (car in-memory) (get-file-stamp pathname)))
                 (cdr in-memory))
                (error-p
                 (error 'missing-component :requires name))
                (t ;; not found: don't keep negative cache, see lp#1335323
                 (unset-asdf-cache-entry `(locate-system ,name))
                 (return-from find-system nil)))))))))
;;;; -------------------------------------------------------------------------
;;;; Finding components

(uiop/package:define-package :asdf/find-component
  (:recycle :asdf/find-component :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/find-system)
  (:export
   #:find-component
   #:resolve-dependency-name #:resolve-dependency-spec
   #:resolve-dependency-combination
   ;; Conditions
   #:missing-component #:missing-component-of-version #:retry
   #:missing-dependency #:missing-dependency-of-version
   #:missing-requires #:missing-parent
   #:missing-required-by #:missing-version))
(in-package :asdf/find-component)

;;;; Missing component conditions

(with-upgradability ()
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
  (defgeneric* (find-component) (base path)
    (:documentation "Find a component by resolving the PATH starting from BASE parent"))
  (defgeneric resolve-dependency-combination (component combinator arguments))

  (defmethod find-component ((base string) path)
    (let ((s (find-system base nil)))
      (and s (find-component s path))))

  (defmethod find-component ((base symbol) path)
    (cond
      (base (find-component (coerce-name base) path))
      (path (find-component path nil))
      (t    nil)))

  (defmethod find-component ((base cons) path)
    (find-component (car base) (cons (cdr base) path)))

  (defmethod find-component ((parent parent-component) (name string))
    (compute-children-by-name parent :only-if-needed-p t) ;; SBCL may miss the u-i-f-r-c method!!!
    (values (gethash name (component-children-by-name parent))))

  (defmethod find-component (base (name symbol))
    (if name
        (find-component base (coerce-name name))
        base))

  (defmethod find-component ((c component) (name cons))
    (find-component (find-component c (car name)) (cdr name)))

  (defmethod find-component ((base t) (actual component))
    actual)

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
              (unset-asdf-cache-entry `(find-system ,name))
              (unset-asdf-cache-entry `(locate-system ,name))))))))


  (defun resolve-dependency-spec (component dep-spec)
    (let ((component (find-component () component)))
      (if (atom dep-spec)
          (resolve-dependency-name component dep-spec)
          (resolve-dependency-combination component (car dep-spec) (cdr dep-spec)))))

  (defmethod resolve-dependency-combination (component combinator arguments)
    (error (compatfmt "~@<Bad dependency ~S for ~S~@:>")
           (cons combinator arguments) component))

  (defmethod resolve-dependency-combination (component (combinator (eql :feature)) arguments)
    (when (featurep (first arguments))
      (resolve-dependency-spec component (second arguments))))

  (defmethod resolve-dependency-combination (component (combinator (eql :version)) arguments)
    (resolve-dependency-name component (first arguments) (second arguments)))) ;; See lp#527788

;;;; -------------------------------------------------------------------------
;;;; Operations

(uiop/package:define-package :asdf/operation
  (:recycle :asdf/operation :asdf/action :asdf) ;; asdf/action for FEATURE pre 2.31.5.
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export
   #:operation
   #:operation-original-initargs #:original-initargs ;; backward-compatibility only. DO NOT USE.
   #:*operations* #:make-operation #:find-operation
   #:feature)) ;; TODO: stop exporting the deprecated FEATURE feature.
(in-package :asdf/operation)

;;; Operation Classes

(when-upgrading (:when (find-class 'operation nil))
  ;; override any obsolete shared-initialize method when upgrading from ASDF2.
  (defmethod shared-initialize :after ((o operation) (slot-names t) &key)
    (values)))

(with-upgradability ()
  (defclass operation ()
    ((original-initargs ;; for backward-compat -- used by GBBopen and swank (via operation-forced)
      :initform nil :initarg :original-initargs :accessor operation-original-initargs)))

  ;; Cache a copy of the INITARGS in the ORIGINAL-INITARGS slot, if that slot is not
  ;; already bound.
  (defmethod initialize-instance :after ((o operation) &rest initargs
                                         &key force force-not system verbose &allow-other-keys)
    (declare (ignore force force-not system verbose))
    (unless (slot-boundp o 'original-initargs)
      (setf (operation-original-initargs o) initargs)))

  (defmethod print-object ((o operation) stream)
    (print-unreadable-object (o stream :type t :identity nil)
      (ignore-errors
       (format stream "~{~S~^ ~}" (operation-original-initargs o))))))

;;; make-operation, find-operation

(with-upgradability ()
  (defparameter* *operations* (make-hash-table :test 'equal))

  (defun make-operation (operation-class &rest initargs)
    (let ((class (coerce-class operation-class
                               :package :asdf/interface :super 'operation :error 'sysdef-error)))
      (ensure-gethash (cons class initargs) *operations*
                      (list* 'make-instance class initargs))))

  (defgeneric find-operation (context spec)
    (:documentation "Find an operation by resolving the SPEC in the CONTEXT"))
  (defmethod find-operation ((context t) (spec operation))
    spec)
  (defmethod find-operation (context (spec symbol))
    (when spec ;; NIL designates itself, i.e. absence of operation
      (apply 'make-operation spec (operation-original-initargs context))))
  (defmethod find-operation (context (spec string))
    (apply 'make-operation spec (operation-original-initargs context)))
  (defmethod operation-original-initargs ((context symbol))
    (declare (ignorable context))
    nil))

;;;; -------------------------------------------------------------------------
;;;; Actions

(uiop/package:define-package :asdf/action
  (:nicknames :asdf-action)
  (:recycle :asdf/action :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system #:asdf/cache :asdf/find-system :asdf/find-component :asdf/operation)
  (:export
   #:action #:define-convenience-action-methods
   #:explain #:action-description
   #:downward-operation #:upward-operation #:sideway-operation #:selfward-operation #:non-propagating-operation
   #:component-depends-on
   #:input-files #:output-files #:output-file #:operation-done-p
   #:action-status #:action-stamp #:action-done-p
   #:component-operation-time #:mark-operation-done #:compute-action-stamp
   #:perform #:perform-with-restarts #:retry #:accept
   #:traverse-actions #:traverse-sub-actions #:required-components ;; in plan
   #:action-path #:find-action #:stamp #:done-p
   #:operation-definition-warning #:operation-definition-error ;; condition
   ))
(in-package :asdf/action)

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute) ;; LispWorks issues spurious warning
  (deftype action () '(cons operation component)) ;; a step to be performed while building

  (deftype operation-designator ()
    ;; an operation designates itself,
    ;; nil designates a context-dependent current operation, and
    ;; class-name or class designates an instance of the designated class.
    '(or operation null symbol class)))

(with-upgradability ()
  (defgeneric traverse-actions (actions &key &allow-other-keys))
  (defgeneric traverse-sub-actions (operation component &key &allow-other-keys))
  (defgeneric required-components (component &key &allow-other-keys)))

;;;; Reified representation for storage or debugging. Note: dropping original-initargs
(with-upgradability ()
  (defun action-path (action)
    (destructuring-bind (o . c) action (cons (type-of o) (component-find-path c))))
  (defun find-action (path)
    (destructuring-bind (o . c) path (cons (make-operation o) (find-component () c)))))


;;;; Convenience methods
(with-upgradability ()
  (defmacro define-convenience-action-methods
      (function formals &key if-no-operation if-no-component operation-initargs)
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
             (let ((,component (find-component () ,component))) ;; do it first, for defsystem-depends-on
               ,(next-method `(safe-read-from-string ,operation :package :asdf/interface) component)))
           (defmethod ,function (,@prefix (,operation symbol) ,component ,@suffix ,@more-args)
             (if ,operation
                 ,(next-method
                   (if operation-initargs ;backward-compatibility with ASDF1's operate. Yuck.
                       `(apply 'make-operation ,operation :original-initargs ,rest ,rest)
                       `(make-operation ,operation))
                   `(or (find-component () ,component) ,if-no-component))
                 ,if-no-operation))
           (defmethod ,function (,@prefix (,operation operation) ,component ,@suffix ,@more-args)
             (if (typep ,component 'component)
                 (error "No defined method for ~S on ~/asdf-action:format-action/"
                        ',function (cons ,operation ,component))
                 (if-let (,found (find-component () ,component))
                    ,(next-method operation found)
                    ,if-no-component))))))))


;;;; self-description
(with-upgradability ()
  (defgeneric action-description (operation component)
    (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))
  (defmethod action-description (operation component)
    (format nil (compatfmt "~@<~A on ~A~@:>")
            (type-of operation) component))
  (defgeneric* (explain) (operation component))
  (defmethod explain ((o operation) (c component))
    (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (action-description o c)))
  (define-convenience-action-methods explain (operation component))

  (defun format-action (stream action &optional colon-p at-sign-p)
    (assert (null colon-p)) (assert (null at-sign-p))
    (destructuring-bind (operation . component) action
      (princ (action-description operation component) stream))))


;;;; Dependencies
(with-upgradability ()
  (defgeneric* (component-depends-on) (operation component) ;; ASDF4: rename to component-dependencies
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

  (defmethod component-depends-on ((o operation) (c component))
    `(;; Normal behavior, to allow user-specified in-order-to dependencies
      ,@(cdr (assoc (type-of o) (component-in-order-to c)))
      ;; For backward-compatibility with ASDF2, any operation that doesn't specify propagation
      ;; or non-propagation through an appropriate mixin will be downward and sideway.
      ,@(unless (typep o '(or downward-operation upward-operation sideway-operation
                              selfward-operation non-propagating-operation))
          `(,@(sideway-operation-depends-on o c)
            ,@(when (typep c 'parent-component) (downward-operation-depends-on o c))))))

  (defmethod downward-operation ((o operation)) nil)
  (defmethod sideway-operation ((o operation)) nil))


;;;---------------------------------------------------------------------------
;;; End of OPERATION class checking
;;;---------------------------------------------------------------------------


;;;; Inputs, Outputs, and invisible dependencies
(with-upgradability ()
  (defgeneric* (output-files) (operation component))
  (defgeneric* (input-files) (operation component))
  (defgeneric* (operation-done-p) (operation component)
    (:documentation "Returns a boolean, which is NIL if the action is forced to be performed again"))
  (define-convenience-action-methods output-files (operation component))
  (define-convenience-action-methods input-files (operation component))
  (define-convenience-action-methods operation-done-p (operation component))

  (defmethod operation-done-p ((o operation) (c component))
    t)

  (defmethod output-files :around (operation component)
    "Translate output files, unless asked not to. Memoize the result."
    operation component ;; hush genera, not convinced by declare ignorable(!)
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

  (defmethod input-files :around (operation component)
    "memoize input files."
    (do-asdf-cache `(input-files ,operation ,component)
      (call-next-method)))

  (defmethod input-files ((o operation) (c component))
    nil)

  (defmethod input-files ((o selfward-operation) (c component))
    `(,@(or (loop :for dep-o :in (ensure-list (selfward-operation o))
                  :append (or (output-files dep-o c) (input-files dep-o c)))
            (if-let ((pathname (component-pathname c)))
              (and (file-pathname-p pathname) (list pathname))))
      ,@(call-next-method))))


;;;; Done performing
(with-upgradability ()
  (defgeneric component-operation-time (operation component)) ;; ASDF4: hide it behind plan-action-stamp
  (define-convenience-action-methods component-operation-time (operation component))

  (defgeneric mark-operation-done (operation component)) ;; ASDF4: hide it behind (setf plan-action-stamp)
  (defgeneric compute-action-stamp (plan operation component &key just-done)
    (:documentation "Has this action been successfully done already,
and at what known timestamp has it been done at or will it be done at?
Takes two keywords JUST-DONE and PLAN:
JUST-DONE is a boolean that is true if the action was just successfully performed,
at which point we want compute the actual stamp and warn if files are missing;
otherwise we are making plans, anticipating the effects of the action.
PLAN is a plan object modelling future effects of actions,
or NIL to denote what actually happened.
Returns two values:
* a STAMP saying when it was done or will be done,
  or T if the action has involves files that need to be recomputed.
* a boolean DONE-P that indicates whether the action has actually been done,
  and both its output-files and its in-image side-effects are up to date."))

  (defclass action-status ()
    ((stamp
      :initarg :stamp :reader action-stamp
      :documentation "STAMP associated with the ACTION if it has been completed already
in some previous image, or T if it needs to be done.")
     (done-p
      :initarg :done-p :reader action-done-p
      :documentation "a boolean, true iff the action was already done (before any planned action)."))
    (:documentation "Status of an action"))

  (defmethod print-object ((status action-status) stream)
    (print-unreadable-object (status stream :type t)
      (with-slots (stamp done-p) status
        (format stream "~@{~S~^ ~}" :stamp stamp :done-p done-p))))

  (defmethod component-operation-time ((o operation) (c component))
    (gethash (type-of o) (component-operation-times c)))

  (defmethod mark-operation-done ((o operation) (c component))
    (setf (gethash (type-of o) (component-operation-times c))
          (compute-action-stamp nil o c :just-done t))))


;;;; Perform
(with-upgradability ()
  (defgeneric* (perform-with-restarts) (operation component))
  (defgeneric* (perform) (operation component))
  (define-convenience-action-methods perform (operation component))

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
       'perform (cons o c))))

  (defmethod perform-with-restarts (operation component)
    ;; TOO verbose, especially as the default. Add your own :before method
    ;; to perform-with-restart or perform if you want that:
    #|(explain operation component)|#
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
  (:intern #:proclamations #:flags)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/find-component :asdf/find-system
   :asdf/operation :asdf/action)
  (:export
   #:try-recompiling
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:basic-load-op #:basic-compile-op #:compile-op-flags #:compile-op-proclamations
   #:load-op #:prepare-op #:compile-op #:test-op #:load-source-op #:prepare-source-op
   #:call-with-around-compile-hook
   #:perform-lisp-compilation #:perform-lisp-load-fasl #:perform-lisp-load-source
   #:lisp-compilation-output-files #:flags))
(in-package :asdf/lisp-action)


;;;; Component classes
(with-upgradability ()
  (defclass cl-source-file (source-file)
    ((type :initform "lisp")))
  (defclass cl-source-file.cl (cl-source-file)
    ((type :initform "cl")))
  (defclass cl-source-file.lsp (cl-source-file)
    ((type :initform "lsp"))))


;;;; Operation classes
(with-upgradability ()
  (defclass basic-load-op (operation) ())
  (defclass basic-compile-op (operation)
    ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
     (flags :initarg :flags :accessor compile-op-flags :initform nil))))

;;; Our default operations: loading into the current lisp image
(with-upgradability ()
  (defclass prepare-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-op :allocation :class))
    (:documentation "Load dependencies necessary for COMPILE-OP or LOAD-OP of a given COMPONENT."))
  (defclass load-op (basic-load-op downward-operation selfward-operation)
    ;; NB: even though compile-op depends on prepare-op it is not needed-in-image-p,
    ;; so we need to directly depend on prepare-op for its side-effects in the current image.
    ((selfward-operation :initform '(prepare-op compile-op) :allocation :class)))
  (defclass compile-op (basic-compile-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-op :allocation :class)))

  (defclass prepare-source-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-source-op :allocation :class)))
  (defclass load-source-op (basic-load-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-source-op :allocation :class)))

  (defclass test-op (selfward-operation)
    ((selfward-operation :initform 'load-op :allocation :class))))


;;;; prepare-op, compile-op and load-op

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
  (defgeneric call-with-around-compile-hook (component thunk))
  (defmethod call-with-around-compile-hook ((c component) function)
    (call-around-hook (around-compile-hook c) function))
  (defun perform-lisp-compilation (o c)
    (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
          ;; we consult input-files, the first of which should be the one to compile-file
          (input-file (first (input-files o c)))
          ;; on some implementations, there are more than one output-file,
          ;; but the first one should always be the primary fasl that gets loaded.
          (outputs (output-files o c)))
      (multiple-value-bind (output warnings-p failure-p)
          (destructuring-bind
              (output-file
               &optional
                 #+(or clasp ecl mkcl) object-file
                 #+clisp lib-file
                 warnings-file) outputs
            (call-with-around-compile-hook
             c #'(lambda (&rest flags)
                   (apply 'compile-file* input-file
                          :output-file output-file
                          :external-format (component-external-format c)
                          :warnings-file warnings-file
                          (append
                           #+clisp (list :lib-file lib-file)
                           #+(or clasp ecl mkcl) (list :object-file object-file)
                           flags (compile-op-flags o))))))
        (check-lisp-compile-results output warnings-p failure-p
                                    "~/asdf-action::format-action/" (list (cons o c))))))

  (defun report-file-p (f)
    (equalp (pathname-type f) "build-report"))
  (defun perform-lisp-warnings-check (o c)
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
  (defmethod perform ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      (perform-lisp-warnings-check o c)))
  (defmethod input-files ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      ;; The most correct way to do it would be to use:
      ;; (traverse-sub-actions o c :other-systems nil :keep-operation 'compile-op :keep-component 'cl-source-file)
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
;;;; Plan

(uiop/package:define-package :asdf/plan
  (:recycle :asdf/plan :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/operation :asdf/system
   :asdf/cache :asdf/find-system :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action)
  (:export
   #:component-operation-time #:mark-operation-done
   #:plan #:plan-traversal #:sequential-plan #:*default-plan-class*
   #:planned-action-status #:plan-action-status #:action-already-done-p
   #:circular-dependency #:circular-dependency-actions
   #:node-for #:needed-in-image-p
   #:action-index #:action-planned-p #:action-valid-p
   #:plan-record-dependency
   #:normalize-forced-systems #:action-forced-p #:action-forced-not-p
   #:map-direct-dependencies #:reduce-direct-dependencies #:direct-dependencies
   #:compute-action-stamp #:traverse-action
   #:circular-dependency #:circular-dependency-actions
   #:call-while-visiting-action #:while-visiting-action
   #:make-plan #:plan-actions #:perform-plan #:plan-operates-on-p
   #:planned-p #:index #:forced #:forced-not #:total-action-count
   #:planned-action-count #:planned-output-action-count #:visited-actions
   #:visiting-action-set #:visiting-action-list #:plan-actions-r
   #:required-components #:filtered-sequential-plan
   #:plan-system
   #:plan-action-filter #:plan-component-type #:plan-keep-operation #:plan-keep-component
   #:traverse-actions #:traverse-sub-actions))
(in-package :asdf/plan)

;;;; Generic plan traversal class
(with-upgradability ()
  (defclass plan () ())
  (defclass plan-traversal (plan)
    ((system :initform nil :initarg :system :accessor plan-system)
     (forced :initform nil :initarg :force :accessor plan-forced)
     (forced-not :initform nil :initarg :force-not :accessor plan-forced-not)
     (total-action-count :initform 0 :accessor plan-total-action-count)
     (planned-action-count :initform 0 :accessor plan-planned-action-count)
     (planned-output-action-count :initform 0 :accessor plan-planned-output-action-count)
     (visited-actions :initform (make-hash-table :test 'equal) :accessor plan-visited-actions)
     (visiting-action-set :initform (make-hash-table :test 'equal) :accessor plan-visiting-action-set)
     (visiting-action-list :initform () :accessor plan-visiting-action-list))))


;;;; Planned action status
(with-upgradability ()
  (defgeneric plan-action-status (plan operation component)
    (:documentation "Returns the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defgeneric (setf plan-action-status) (new-status plan operation component)
    (:documentation "Sets the ACTION-STATUS associated to
the action of OPERATION on COMPONENT in the PLAN"))

  (defclass planned-action-status (action-status)
    ((planned-p
      :initarg :planned-p :reader action-planned-p
      :documentation "a boolean, true iff the action was included in the plan.")
     (index
      :initarg :index :reader action-index
      :documentation "an integer, counting all traversed actions in traversal order."))
    (:documentation "Status of an action in a plan"))

  (defmethod print-object ((status planned-action-status) stream)
    (print-unreadable-object (status stream :type t :identity nil)
      (with-slots (stamp done-p planned-p index) status
        (format stream "~@{~S~^ ~}" :stamp stamp :done-p done-p :planned-p planned-p :index index))))

  (defmethod action-planned-p ((action-status t))
    t) ; default method for non planned-action-status objects

  ;; TODO: eliminate NODE-FOR, use CONS.
  ;; Supposes cleaner protocol for operation initargs passed to MAKE-OPERATION.
  ;; However, see also component-operation-time and mark-operation-done
  (defun node-for (o c) (cons (type-of o) c))

  (defun action-already-done-p (plan operation component)
    (action-done-p (plan-action-status plan operation component)))

  (defmethod plan-action-status ((plan null) (o operation) (c component))
    (multiple-value-bind (stamp done-p) (component-operation-time o c)
      (make-instance 'action-status :stamp stamp :done-p done-p)))

  (defmethod (setf plan-action-status) (new-status (plan null) (o operation) (c component))
    (let ((to (type-of o))
          (times (component-operation-times c)))
      (if (action-done-p new-status)
          (remhash to times)
          (setf (gethash to times) (action-stamp new-status))))
    new-status))


;;;; forcing
(with-upgradability ()
  (defgeneric action-forced-p (plan operation component))
  (defgeneric action-forced-not-p (plan operation component))

  (defun normalize-forced-systems (x system)
    (etypecase x
      ((or (member nil :all) hash-table function) x)
      (cons (list-to-hash-set (mapcar #'coerce-name x)))
      ((eql t) (when system (list-to-hash-set (list (coerce-name system)))))))

  (defun normalize-forced-not-systems (x system)
    (let ((requested
            (etypecase x
              ((or (member nil :all) hash-table function) x)
              (cons (list-to-hash-set (mapcar #'coerce-name x)))
              ((eql t) (if system (let ((name (coerce-name system)))
                                    #'(lambda (x) (not (equal x name))))
                           t)))))
      (if (and *immutable-systems* requested)
          #'(lambda (x) (or (call-function requested x) (call-function *immutable-systems* x)))
          (or *immutable-systems* requested))))

  (defun action-override-p (plan operation component override-accessor)
    (declare (ignore operation))
    (call-function (funcall override-accessor plan)
                   (coerce-name (component-system (find-component () component)))))

  (defmethod action-forced-p (plan operation component)
    (and
     ;; Did the user ask us to re-perform the action?
     (action-override-p plan operation component 'plan-forced)
     ;; You really can't force a builtin system and :all doesn't apply to it,
     ;; except it it's the specifically the system currently being built.
     (not (let ((system (component-system component)))
            (and (builtin-system-p system)
                 (not (eq system (plan-system plan))))))))

  (defmethod action-forced-not-p (plan operation component)
    ;; Did the user ask us to not re-perform the action?
    ;; NB: force-not takes precedence over force, as it should
    (action-override-p plan operation component 'plan-forced-not))

  (defmethod action-forced-p ((plan null) (operation operation) (component component))
    nil)

  (defmethod action-forced-not-p ((plan null) (operation operation) (component component))
    nil))


;;;; action-valid-p
(with-upgradability ()
  (defgeneric action-valid-p (plan operation component)
    (:documentation "Is this action valid to include amongst dependencies?"))
  (defmethod action-valid-p ((plan t) (o operation) (c component))
    (if-let (it (component-if-feature c)) (featurep it) t))
  (defmethod action-valid-p ((plan t) (o null) (c t)) nil)
  (defmethod action-valid-p ((plan t) (o t) (c null)) nil)
  (defmethod action-valid-p ((plan null) (o operation) (c component)) t))

;;;; Is the action needed in this image?
(with-upgradability ()
  (defgeneric needed-in-image-p (operation component)
    (:documentation "Is the action of OPERATION on COMPONENT needed in the current image to be meaningful,
    or could it just as well have been done in another Lisp image?"))

  (defmethod needed-in-image-p ((o operation) (c component))
    ;; We presume that actions that modify the filesystem don't need be run
    ;; in the current image if they have already been done in another,
    ;; and can be run in another process (e.g. a fork),
    ;; whereas those that don't are meant to side-effect the current image and can't.
    (not (output-files o c))))


;;;; Visiting dependencies of an action and computing action stamps
(with-upgradability ()
  (defun (map-direct-dependencies) (plan operation component fun)
    (loop* :for (dep-o-spec . dep-c-specs) :in (component-depends-on operation component)
           :for dep-o = (find-operation operation dep-o-spec)
           :when dep-o
           :do (loop :for dep-c-spec :in dep-c-specs
                     :for dep-c = (and dep-c-spec (resolve-dependency-spec component dep-c-spec))
                     :when (and dep-c (action-valid-p plan dep-o dep-c))
                       :do (funcall fun dep-o dep-c))))

  (defun (reduce-direct-dependencies) (plan operation component combinator seed)
    (map-direct-dependencies
     plan operation component
     #'(lambda (dep-o dep-c)
         (setf seed (funcall combinator dep-o dep-c seed))))
    seed)

  (defun (direct-dependencies) (plan operation component)
    (reduce-direct-dependencies plan operation component #'acons nil))

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
    ;;   or T is either hasn't been done or is out of date.
    ;; 2- the DONE-IN-IMAGE-P boolean flag that is T if the action has already been done
    ;;   in the current image, or NIL if it hasn't.
    ;; Note that if e.g. LOAD-OP only depends on up-to-date files, but
    ;; hasn't been done in the current image yet, then it can have a non-T timestamp,
    ;; yet a NIL done-in-image-p flag.
    (nest
     (block ())
     (let ((dep-stamp ; collect timestamp from dependencies (or T if forced or out-of-date)
             (reduce-direct-dependencies
              plan o c
              #'(lambda (o c stamp)
                  (if-let (it (plan-action-status plan o c))
                    (latest-stamp stamp (action-stamp it))
                    t))
              nil)))
       ;; out-of-date dependency: don't bother expensively querying the filesystem
       (when (and (eq dep-stamp t) (not just-done)) (return (values t nil))))
     ;; collect timestamps from inputs, and exit early if any is missing
     (let* ((in-files (input-files o c))
            (in-stamps (mapcar #'get-file-stamp in-files))
            (missing-in (loop :for f :in in-files :for s :in in-stamps :unless s :collect f))
            (latest-in (stamps-latest (cons dep-stamp in-stamps))))
       (when (and missing-in (not just-done)) (return (values t nil))))
     ;; collect timestamps from outputs, and exit early if any is missing
     (let* ((out-files (output-files o c))
            (out-stamps (mapcar (if just-done 'register-file-stamp 'get-file-stamp) out-files))
            (missing-out (loop :for f :in out-files :for s :in out-stamps :unless s :collect f))
            (earliest-out (stamps-earliest out-stamps)))
       (when (and missing-out (not just-done)) (return (values t nil))))
     (let* (;; There are three kinds of actions:
            (out-op (and out-files t)) ; those that create files on the filesystem
            ;;(image-op (and in-files (null out-files))) ; those that load stuff into the image
            ;;(null-op (and (null out-files) (null in-files))) ; placeholders that do nothing
            ;; When was the thing last actually done? (Now, or ask.)
            (op-time (or just-done (component-operation-time o c)))
            ;; Time stamps from the files at hand, and whether any is missing
            (all-present (not (or missing-in missing-out)))
            ;; Has any input changed since we last generated the files?
            (up-to-date-p (stamp<= latest-in earliest-out))
            ;; If everything is up to date, the latest of inputs and outputs is our stamp
            (done-stamp (stamps-latest (cons latest-in out-stamps))))
       ;; Warn if some files are missing:
       ;; either our model is wrong or some other process is messing with our files.
       (when (and just-done (not all-present))
         (warn "~A completed without ~:[~*~;~*its input file~:p~2:*~{ ~S~}~*~]~
                ~:[~; or ~]~:[~*~;~*its output file~:p~2:*~{ ~S~}~*~]"
               (action-description o c)
               missing-in (length missing-in) (and missing-in missing-out)
               missing-out (length missing-out))))
     ;; Note that we use stamp<= instead of stamp< to play nice with generated files.
     ;; Any race condition is intrinsic to the limited timestamp resolution.
     (if (or just-done ;; The done-stamp is valid: if we're just done, or
             ;; if all filesystem effects are up-to-date and there's no invalidating reason.
             (and all-present up-to-date-p (operation-done-p o c) (not (action-forced-p plan o c))))
         (values done-stamp ;; return the hard-earned timestamp
                 (or just-done
                     out-op ;; a file-creating op is done when all files are up to date
                     ;; a image-effecting a placeholder op is done when it was actually run,
                     (and op-time (eql op-time done-stamp)))) ;; with the matching stamp
         ;; done-stamp invalid: return a timestamp in an indefinite future, action not done yet
         (values t nil)))))


;;;; Generic support for plan-traversal
(with-upgradability ()
  (defgeneric plan-record-dependency (plan operation component))

  (defgeneric call-while-visiting-action (plan operation component function)
    (:documentation "Detect circular dependencies"))

  (defmethod initialize-instance :after ((plan plan-traversal)
                                         &key force force-not system
                                         &allow-other-keys)
    (with-slots (forced forced-not) plan
      (setf forced (normalize-forced-systems force system))
      (setf forced-not (normalize-forced-not-systems force-not system))))

  (defmethod (setf plan-action-status) (new-status (plan plan-traversal) (o operation) (c component))
    (setf (gethash (node-for o c) (plan-visited-actions plan)) new-status))

  (defmethod plan-action-status ((plan plan-traversal) (o operation) (c component))
    (or (and (action-forced-not-p plan o c) (plan-action-status nil o c))
        (values (gethash (node-for o c) (plan-visited-actions plan)))))

  (defmethod action-valid-p ((plan plan-traversal) (o operation) (s system))
    (and (not (action-forced-not-p plan o s)) (call-next-method)))

  (defmethod call-while-visiting-action ((plan plan-traversal) operation component fun)
    (with-accessors ((action-set plan-visiting-action-set)
                     (action-list plan-visiting-action-list)) plan
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


;;;; Actual traversal: traverse-action
(with-upgradability ()
  (define-condition circular-dependency (system-definition-error)
    ((actions :initarg :actions :reader circular-dependency-actions))
    (:report (lambda (c s)
               (format s (compatfmt "~@<Circular dependency: ~3i~_~S~@:>")
                       (circular-dependency-actions c)))))

  (defmacro while-visiting-action ((p o c) &body body)
    `(call-while-visiting-action ,p ,o ,c #'(lambda () ,@body)))

  (defgeneric traverse-action (plan operation component needed-in-image-p))

  ;; TRAVERSE-ACTION, in the context of a given PLAN object that accumulates dependency data,
  ;; visits the action defined by its OPERATION and COMPONENT arguments,
  ;; and all its transitive dependencies (unless already visited),
  ;; in the context of the action being (or not) NEEDED-IN-IMAGE-P,
  ;; i.e. needs to be done in the current image vs merely have been done in a previous image.
  ;; For actions that are up-to-date, it returns a STAMP identifying the state of the action
  ;; (that's timestamp, but it could be a cryptographic digest in some ASDF extension),
  ;; or T if the action needs to be done again.
  ;;
  ;; Note that for an XCVB-like plan with one-image-per-file-outputting-action,
  ;; the below method would be insufficient, since it assumes a single image
  ;; to traverse each node at most twice; non-niip actions would be traversed only once,
  ;; but niip nodes could be traversed once per image, i.e. once plus once per non-niip action.

  (defmethod traverse-action (plan operation component needed-in-image-p)
    (block nil
      ;; ACTION-VALID-P among other things, handles forcing logic, including FORCE-NOT,
      ;; and IF-FEATURE filtering.
      (unless (action-valid-p plan operation component) (return nil))
      ;; the following hook is needed by POIU, which tracks a full dependency graph,
      ;; instead of just a dependency order as in vanilla ASDF
      (plan-record-dependency plan operation component)
      ;; needed in image distinguishes b/w things that must happen in the
      ;; current image and those things that simply need to have been done in a previous one.
      (let* ((aniip (needed-in-image-p operation component)) ; action-specific needed-in-image
             ;; effective niip: meaningful for the action and required by the plan as traversed
             (eniip (and aniip needed-in-image-p))
             ;; status: have we traversed that action previously, and if so what was its status?
             (status (plan-action-status plan operation component)))
        (when (and status (or (action-done-p status) (action-planned-p status) (not eniip)))
          (return (action-stamp status))) ; Already visited with sufficient need-in-image level!
        (labels ((visit-action (niip) ; We may visit the action twice, once with niip NIL, then T
                   (map-direct-dependencies ; recursively traverse dependencies
                    plan operation component #'(lambda (o c) (traverse-action plan o c niip)))
                   (multiple-value-bind (stamp done-p) ; AFTER dependencies have been traversed,
                       (compute-action-stamp plan operation component) ; compute action stamp
                     (let ((add-to-plan-p (or (eql stamp t) (and niip (not done-p)))))
                       (cond ; it needs be done if it's out of date or needed in image but absent
                         ((and add-to-plan-p (not niip)) ; if we need to do it,
                          (visit-action t)) ; then we need to do it *in the (current) image*!
                         (t
                          (setf (plan-action-status plan operation component) ; update status:
                                (make-instance
                                 'planned-action-status
                                 :stamp stamp ; computed stamp
                                 :done-p (and done-p (not add-to-plan-p)) ; done *and* up-to-date?
                                 :planned-p add-to-plan-p ; included in list of things to be done?
                                 :index (if status ; index of action amongst all nodes in traversal
                                            (action-index status) ;; if already visited, keep index
                                            (incf (plan-total-action-count plan))))) ; else new index
                          (when add-to-plan-p ; if it needs to be added to the plan,
                            (incf (plan-planned-action-count plan)) ; count it
                            (unless aniip ; if it's output-producing,
                              (incf (plan-planned-output-action-count plan)))) ; count it
                          stamp)))))) ; return the stamp
          (while-visiting-action (plan operation component) ; maintain context, handle circularity.
            (visit-action eniip))))))) ; visit the action


;;;; Sequential plans (the default)
(with-upgradability ()
  (defclass sequential-plan (plan-traversal)
    ((actions-r :initform nil :accessor plan-actions-r)))

  (defgeneric plan-actions (plan))
  (defmethod plan-actions ((plan list))
    plan)
  (defmethod plan-actions ((plan sequential-plan))
    (reverse (plan-actions-r plan)))

  (defmethod plan-record-dependency ((plan sequential-plan) (o operation) (c component))
    (values))

  (defmethod (setf plan-action-status) :after
      (new-status (p sequential-plan) (o operation) (c component))
    (when (action-planned-p new-status)
      (push (cons o c) (plan-actions-r p)))))

;;;; High-level interface: traverse, perform-plan, plan-operates-on-p
(with-upgradability ()
  (defgeneric make-plan (plan-class operation component &key &allow-other-keys)
    (:documentation
     "Generate and return a plan for performing OPERATION on COMPONENT."))
  (define-convenience-action-methods make-plan (plan-class operation component &key))

  (defgeneric perform-plan (plan &key))
  (defgeneric plan-operates-on-p (plan component))

  (defvar *default-plan-class* 'sequential-plan)

  (defmethod make-plan (plan-class (o operation) (c component) &rest keys &key &allow-other-keys)
    (let ((plan (apply 'make-instance (or plan-class *default-plan-class*)
                       :system (component-system c) keys)))
      (traverse-action plan o c t)
      plan))

  (defmethod perform-plan :around ((plan t) &key)
    #+xcl (declare (ignorable plan))
    (let ((*package* *package*)
          (*readtable* *readtable*))
      (with-compilation-unit () ;; backward-compatibility.
        (call-next-method))))   ;; Going forward, see deferred-warning support in lisp-build.

  (defmethod perform-plan ((plan t) &rest keys &key &allow-other-keys)
    (apply 'perform-plan (plan-actions plan) keys))

  (defmethod perform-plan ((steps list) &key force &allow-other-keys)
    (loop* :for (o . c) :in steps
           :when (or force (not (nth-value 1 (compute-action-stamp nil o c))))
           :do (perform-with-restarts o c)))

  (defmethod plan-operates-on-p ((plan plan-traversal) (component-path list))
    (plan-operates-on-p (plan-actions plan) component-path))

  (defmethod plan-operates-on-p ((plan list) (component-path list))
    (find component-path (mapcar 'cdr plan)
          :test 'equal :key 'component-find-path)))


;;;; Incidental traversals

;;; Making a FILTERED-SEQUENTIAL-PLAN can be used to, e.g., all of the source
;;; files required by a bundling operation.
(with-upgradability ()
  (defclass filtered-sequential-plan (sequential-plan)
    ((action-filter :initform t :initarg :action-filter :reader plan-action-filter)
     (component-type :initform t :initarg :component-type :reader plan-component-type)
     (keep-operation :initform t :initarg :keep-operation :reader plan-keep-operation)
     (keep-component :initform t :initarg :keep-component :reader plan-keep-component)))

  (defmethod initialize-instance :after ((plan filtered-sequential-plan)
                                         &key force force-not
                                           other-systems)
    (declare (ignore force force-not))
    (with-slots (forced forced-not action-filter system) plan
      (setf forced (normalize-forced-systems (if other-systems :all t) system))
      (setf forced-not (normalize-forced-not-systems (if other-systems nil t) system))
      (setf action-filter (ensure-function action-filter))))

  (defmethod action-valid-p ((plan filtered-sequential-plan) o c)
    (and (funcall (plan-action-filter plan) o c)
         (typep c (plan-component-type plan))
         (call-next-method)))

  (defmethod traverse-actions (actions &rest keys &key plan-class &allow-other-keys)
    (let ((plan (apply 'make-instance (or plan-class 'filtered-sequential-plan) keys)))
      (loop* :for (o . c) :in actions :do (traverse-action plan o c t))
      plan))

  (define-convenience-action-methods traverse-sub-actions (operation component &key))
  (defmethod traverse-sub-actions ((operation operation) (component component)
                                   &rest keys &key &allow-other-keys)
    (apply 'traverse-actions (direct-dependencies t operation component)
           :system (component-system component) keys))

  (defmethod plan-actions ((plan filtered-sequential-plan))
    (with-slots (keep-operation keep-component) plan
      (loop* :for (o . c) :in (call-next-method)
             :when (and (typep o keep-operation) (typep c keep-component))
             :collect (cons o c))))

  (defmethod required-components (system &rest keys &key (goal-operation 'load-op) &allow-other-keys)
    (remove-duplicates
     (mapcar 'cdr (plan-actions
                   (apply 'traverse-sub-actions goal-operation system
                          (remove-plist-key :goal-operation keys))))
     :from-end t)))

;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(uiop/package:define-package :asdf/operate
  (:recycle :asdf/operate :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/operation :asdf/action
   :asdf/find-system :asdf/find-component :asdf/lisp-action :asdf/plan)
  (:export
   #:operate #:oos
   #:*systems-being-operated*
   #:build-op #:make
   #:load-system #:load-systems #:load-systems*
   #:compile-system #:test-system #:require-system
   #:*load-system-operation* #:module-provide-asdf
   #:component-loaded-p #:already-loaded-systems))
(in-package :asdf/operate)

(with-upgradability ()
  (defgeneric* (operate) (operation component &key &allow-other-keys)
    (:documentation
     "Operate does three things:

1. It creates an instance of OPERATION-CLASS using any keyword parameters as initargs.
2. It finds the  asdf-system specified by SYSTEM (possibly loading it from disk).
3. It then calls MAKE-PLAN with the operation and system as arguments

The operation of making a plan is wrapped in WITH-COMPILATION-UNIT and error
handling code.  If a VERSION argument is supplied, then operate also ensures
that the system found satisfies it using the VERSION-SATISFIES method.

Note that dependencies may cause the operation to invoke other operations on the system
or its components: the new operations will be created with the same initargs as the original one.

The :FORCE or :FORCE-NOT argument to OPERATE can be:
  T to force the inside of the specified system to be rebuilt (resp. not),
    without recursively forcing the other systems we depend on.
  :ALL to force all systems including other systems we depend on to be rebuilt (resp. not).
  (SYSTEM1 SYSTEM2 ... SYSTEMN) to force systems named in a given list
:FORCE has precedence over :FORCE-NOT; builtin systems cannot be forced."))

  (define-convenience-action-methods
      operate (operation component &key)
      ;; I'd like to at least remove-plist-keys :force :force-not :verbose,
      ;; but swank.asd relies on :force (!).
      :operation-initargs t ;; backward-compatibility with ASDF1. Yuck.
      :if-no-component (error 'missing-component :requires component))

  (defvar *systems-being-operated* nil
    "A boolean indicating that some systems are being operated on")

  (defmethod operate :around (operation component &rest keys
                              &key verbose
                                (on-warnings *compile-file-warnings-behaviour*)
                                (on-failure *compile-file-failure-behaviour*) &allow-other-keys)
    (let* ((systems-being-operated *systems-being-operated*)
           (*systems-being-operated* (or systems-being-operated (make-hash-table :test 'equal)))
           (operation-remaker ;; how to remake the operation after ASDF was upgraded (if it was)
             (etypecase operation
               (operation (let ((name (type-of operation))
                                (initargs (operation-original-initargs operation)))
                            #'(lambda () (apply 'make-operation name :original-initargs initargs initargs))))
               ((or symbol string) (constantly operation))))
           (component-path (typecase component ;; to remake the component after ASDF upgrade
                             (component (component-find-path component))
                             (t component))))
      ;; Before we operate on any system, make sure ASDF is up-to-date,
      ;; for if an upgrade is ever attempted at any later time, there may be BIG trouble.
      (unless systems-being-operated
        (when (upgrade-asdf)
          ;; If we were upgraded, restart OPERATE the hardest of ways, for
          ;; its function may have been redefined, its symbol uninterned, its package deleted.
          (return-from operate
            (apply 'operate (funcall operation-remaker) component-path keys))))
      ;; Setup proper bindings around any operate call.
      (with-asdf-cache ()
        (let* ((*verbose-out* (and verbose *standard-output*))
               (*compile-file-warnings-behaviour* on-warnings)
               (*compile-file-failure-behaviour* on-failure))
          (call-next-method)))))

  (defmethod operate :before ((operation operation) (component component)
                              &key version &allow-other-keys)
    (let ((system (component-system component)))
      (setf (gethash (coerce-name system) *systems-being-operated*) system))
    (unless (version-satisfies component version)
      (error 'missing-component-of-version :requires component :version version)))

  (defmethod operate ((operation operation) (component component)
                      &rest keys &key plan-class &allow-other-keys)
    (let ((plan (apply 'make-plan plan-class operation component keys)))
      (apply 'perform-plan plan keys)
      (values operation plan)))

  (defun oos (operation component &rest args &key &allow-other-keys)
    (apply 'operate operation component args))

  (setf (documentation 'oos 'function)
        (format nil "Short for _operate on system_ and an alias for the OPERATE function.~%~%~a"
                (documentation 'operate 'function))))


;;;; Common operations
(with-upgradability ()
  (defvar *load-system-operation* 'load-op
    "Operation used by ASDF:LOAD-SYSTEM. By default, ASDF:LOAD-OP.
You may override it with e.g. ASDF:LOAD-FASL-OP from asdf-bundle
or ASDF:LOAD-SOURCE-OP if your fasl loading is somehow broken.

The default operation may change in the future if we implement a
component-directed strategy for how to load or compile systems.")

  (defmethod component-depends-on ((o prepare-op) (s system))
    (loop :for (o . cs) :in (call-next-method)
          :collect (cons (if (eq o 'load-op) *load-system-operation* o) cs)))

  (defclass build-op (non-propagating-operation) ()
    (:documentation "Since ASDF3, BUILD-OP is the recommended 'master' operation,
to operate by default on a system or component, via the function BUILD.
Its meaning is configurable via the :BUILD-OPERATION option of a component.
which typically specifies the name of a specific operation to which to delegate the build,
as a symbol or as a string later read as a symbol (after loading the defsystem-depends-on);
if NIL is specified (the default), BUILD-OP falls back to the *LOAD-SYSTEM-OPERATION*
that will load the system in the current image, and its typically LOAD-OP."))
  (defmethod component-depends-on ((o build-op) (c component))
    `((,(or (component-build-operation c) *load-system-operation*) ,c)
      ,@(call-next-method)))

  (defun make (system &rest keys)
    "The recommended way to interact with ASDF3.1 is via (ASDF:MAKE :FOO).
It will build system FOO using the operation BUILD-OP,
the meaning of which is configurable by the system, and
defaults to *LOAD-SYSTEM-OPERATION*, usually LOAD-OP,
to load it in current image."
    (apply 'operate 'build-op system keys)
    t)

  (defun load-system (system &rest keys &key force force-not verbose version &allow-other-keys)
    "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for details."
    (declare (ignore force force-not verbose version))
    (apply 'operate *load-system-operation* system keys)
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
    "has given COMPONENT been successfully loaded in the current image (yet)?"
    (action-already-done-p nil (make-instance 'load-op) (find-component component ())))

  (defun already-loaded-systems ()
    "return a list of the names of the systems that have been successfully loaded so far"
    (remove-if-not 'component-loaded-p (registered-systems)))

  (defun require-system (system &rest keys &key &allow-other-keys)
    "Ensure the specified SYSTEM is loaded, passing the KEYS to OPERATE, but skip any update to the
system or its dependencies if they have already been loaded."
    (apply 'load-system system :force-not (already-loaded-systems) keys)))


;;;; Define the class REQUIRE-SYSTEM, to be hooked into CL:REQUIRE when possible,
;; i.e. for ABCL, CLISP, ClozureCL, CMUCL, ECL, MKCL and SBCL
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
    (unless (length=n-p arguments 1)
      (error (compatfmt "~@<Bad dependency ~S for ~S. ~S takes only one argument~@:>")
             (cons combinator arguments) component combinator))
    (let* ((module (car arguments))
           (name (string-downcase module))
           (system (find-system name nil)))
      (assert module)
      ;;(unless (typep system '(or null require-system))
      ;;  (warn "~S depends on ~S but ~S is registered as a ~S"
      ;;        component (cons combinator arguments) module (type-of system)))
      (or system (let ((system (make-instance 'require-system :name name)))
                   (register-system system)
                   system))))

  (defun module-provide-asdf (name)
    (let ((module (string-downcase name)))
      (unless (member module *modules-being-required* :test 'equal)
        (let ((*modules-being-required* (cons module *modules-being-required*))
              #+sbcl (sb-impl::*requiring* (remove module sb-impl::*requiring* :test 'equal)))
          (handler-bind
              ((style-warning #'muffle-warning)
               (missing-component (constantly nil))
               (error #'(lambda (e)
                          (format *error-output* (compatfmt "~@<ASDF could not load ~(~A~) because ~A.~@:>~%")
                                  name e))))
            (let ((*verbose-out* (make-broadcast-stream)))
              (let ((system (find-system module nil)))
                (when system
                  (require-system system :verbose nil)
                  t)))))))))


;;;; Some upgrade magic
(with-upgradability ()
  (defun restart-upgraded-asdf ()
    ;; If we're in the middle of something, restart it.
    (when *asdf-cache*
      (let ((l (loop :for k :being :the hash-keys :of *asdf-cache*
                     :when (eq (first k) 'find-system) :collect (second k))))
        (clrhash *asdf-cache*)
        (dolist (s l) (find-system s nil)))))
  (register-hook-function '*post-upgrade-restart-hook* 'restart-upgraded-asdf))


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

(when-upgrading () (undefine-function '(setf output-translations)))

(with-upgradability ()
  (define-condition invalid-output-translation (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid asdf output-translation ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  (defvar *output-translations* ()
    "Either NIL (for uninitialized), or a list of one element,
said element itself being a sorted list of mappings.
Each mapping is a pair of a source pathname and destination pathname,
and the order is by decreasing length of namestring of the source pathname.")

  (defun output-translations ()
    (car *output-translations*))

  (defun set-output-translations (new-value)
    (setf *output-translations*
          (list
           (stable-sort (copy-list new-value) #'>
                        :key #'(lambda (x)
                                 (etypecase (car x)
                                   ((eql t) -1)
                                   (pathname
                                    (let ((directory (pathname-directory (car x))))
                                      (if (listp directory) (length directory) 0))))))))
    new-value)
  (defun* ((setf output-translations)) (new-value) (set-output-translations new-value))

  (defun output-translations-initialized-p ()
    (and *output-translations* t))

  (defun clear-output-translations ()
    "Undoes any initialization of the output translations."
    (setf *output-translations* '())
    (values))
  (register-clear-configuration-hook 'clear-output-translations)

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

  (defparameter* *default-output-translations*
    '(environment-output-translations
      user-output-translations-pathname
      user-output-translations-directory-pathname
      system-output-translations-pathname
      system-output-translations-directory-pathname))

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

  (defparameter *output-translations-file* (parse-unix-namestring "common-lisp/asdf-output-translations.conf"))
  (defparameter *output-translations-directory* (parse-unix-namestring "common-lisp/asdf-output-translations.conf.d/"))

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

  (defun compute-output-translations (&optional parameter)
    "read the configuration, return it"
    (remove-duplicates
     (while-collecting (c)
       (inherit-output-translations
        `(wrapping-output-translations ,parameter ,@*default-output-translations*) :collect #'c))
     :test 'equal :from-end t))

  (defvar *output-translations-parameter* nil)

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

  #+abcl
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
  (:recycle :asdf/source-registry :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
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
   #:environment-source-registry #:process-source-registry
   #:compute-source-registry #:flatten-source-registry
   #:sysdef-source-registry-search))
(in-package :asdf/source-registry)

(with-upgradability ()
  (define-condition invalid-source-registry (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  ;; Using ack 1.2 exclusions
  (defvar *default-source-registry-exclusions*
    '(".bzr" ".cdv"
      ;; "~.dep" "~.dot" "~.nib" "~.plst" ; we don't support ack wildcards
      ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
      "_sgbak" "autom4te.cache" "cover_db" "_build"
      "debian")) ;; debian often builds stuff under the debian directory... BAD.

  (defvar *source-registry-exclusions* *default-source-registry-exclusions*)

  (defvar *source-registry* nil
    "Either NIL (for uninitialized), or an equal hash-table, mapping
system names to pathnames of .asd files")

  (defun source-registry-initialized-p ()
    (typep *source-registry* 'hash-table))

  (defun clear-source-registry ()
    "Undoes any initialization of the source registry."
    (setf *source-registry* nil)
    (values))
  (register-clear-configuration-hook 'clear-source-registry)

  (defparameter *wild-asd*
    (make-pathname* :directory nil :name *wild* :type "asd" :version :newest))

  (defun directory-asd-files (directory)
    (directory-files directory *wild-asd*))

  (defun collect-asds-in-directory (directory collect)
    (let ((asds (directory-asd-files directory)))
      (map () collect asds)
      asds))

  (defvar *recurse-beyond-asds* t
    "Should :tree entries of the source-registry recurse in subdirectories
after having found a .asd file? True by default.")

  (defun process-source-registry-cache (directory collect)
    (let ((cache (ignore-errors
                  (safe-read-file-form (subpathname directory ".cl-source-registry.cache")))))
      (when (and (listp cache) (eq :source-registry-cache (first cache)))
        (loop :for s :in (rest cache) :do (funcall collect (subpathname directory s)))
        t)))

  (defun collect-sub*directories-asd-files
      (directory &key (exclude *default-source-registry-exclusions*) collect
                   (recurse-beyond-asds *recurse-beyond-asds*) ignore-cache)
    (collect-sub*directories
     directory
     #'(lambda (dir)
         (unless (and (not ignore-cache) (process-source-registry-cache directory collect))
           (let ((asds (collect-asds-in-directory dir collect)))
             (or recurse-beyond-asds (not asds)))))
     #'(lambda (x)
         (not (member (car (last (pathname-directory x))) exclude :test #'equal)))
     (constantly nil)))

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
      #+mkcl (:tree ,(translate-logical-pathname "CONTRIB:"))
      #+cmu (:tree #p"modules:")
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

  (defgeneric* (process-source-registry) (spec &key inherit register))

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

  (defun flatten-source-registry (&optional parameter)
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

  ;; Will read the configuration and initialize all internal variables.
  (defun compute-source-registry (&optional parameter (registry *source-registry*))
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
                 (cond
                   ((gethash name registry) ; already shadowed by something else
                    nil)
                   ((gethash name h) ; conflict at current level
                    (when *verbose-out*
                      (warn (compatfmt "~@<In source-registry entry ~A~@[/~*~] ~
                                found several entries for ~A - picking ~S over ~S~:>")
                            directory recurse name (gethash name h) asd)))
                   (t
                    (setf (gethash name registry) asd)
                    (setf (gethash name h) asd))))))
          h)))
    (values))

  (defvar *source-registry-parameter* nil)

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
;;;; Defsystem

(uiop/package:define-package :asdf/parse-defsystem
  (:recycle :asdf/parse-defsystem :asdf/defsystem :asdf)
  (:nicknames :asdf/defsystem) ;; previous name, to be compatible with, in case anyone cares
  (:use :uiop/common-lisp :asdf/driver :asdf/upgrade
   :asdf/cache :asdf/component :asdf/system
   :asdf/find-system :asdf/find-component :asdf/action :asdf/lisp-action :asdf/operate)
  (:import-from :asdf/system #:depends-on #:weakly-depends-on)
  (:export
   #:defsystem #:register-system-definition
   #:class-for-type #:*default-component-class*
   #:determine-system-directory #:parse-component-form
   #:non-toplevel-system #:non-system-system
   #:sysdef-error-component #:check-component-input))
(in-package :asdf/parse-defsystem)

;;; Pathname
(with-upgradability ()
  (defun determine-system-directory (pathname)
    ;; The defsystem macro calls this function to determine
    ;; the pathname of a system as follows:
    ;; 1. if the pathname argument is an pathname object (NOT a namestring),
    ;;    that is already an absolute pathname, return it.
    ;; 2. otherwise, the directory containing the LOAD-PATHNAME
    ;;    is considered (as deduced from e.g. *LOAD-PATHNAME*), and
    ;;    if it is indeed available and an absolute pathname, then
    ;;    the PATHNAME argument is normalized to a relative pathname
    ;;    as per PARSE-UNIX-NAMESTRING (with ENSURE-DIRECTORY T)
    ;;    and merged into that DIRECTORY as per SUBPATHNAME.
    ;;    Note: avoid *COMPILE-FILE-PATHNAME* because .asd is loaded,
    ;;    and may be from within the EVAL-WHEN of a file compilation.
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
                         (safe-read-file-form (subpathname pathname subpath)
                                              :at at :package :asdf-user)))
                      ((:read-file-line)
                       (destructuring-bind (subpath &key (at 0)) (rest form)
                         (safe-read-file-line (subpathname pathname subpath)
                                              :at at)))
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
  (defun* parse-dependency-def (dd)
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

  (defun* parse-dependency-defs (dd-list)
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
    (with-asdf-cache ()
      (let* ((name (coerce-name name))
             (source-file (if sfp source-file (resolve-symlinks* (load-pathname))))
             (registered (system-registered-p name))
             (registered! (if registered
                              (rplaca registered (get-file-stamp source-file))
                              (register-system
                               (make-instance 'system :name name :source-file source-file))))
             (system (reset-system (cdr registered!)
                                   :name name :source-file source-file))
             (component-options
              (remove-plist-keys '(:defsystem-depends-on :class) options))
             (defsystem-dependencies (loop :for spec :in defsystem-depends-on
                                           :when (resolve-dependency-spec nil spec)
                                           :collect :it)))
        ;; cache defsystem-depends-on in canonical form
        (when defsystem-depends-on
          (setf component-options
                (append `(:defsystem-depends-on ,(parse-dependency-defs defsystem-depends-on))
                        component-options)))
        (set-asdf-cache-entry `(find-system ,name) (list system))
        (load-systems* defsystem-dependencies)
        ;; We change-class AFTER we loaded the defsystem-depends-on
        ;; since the class might be defined as part of those.
        (let ((class (class-for-type nil class)))
          (unless (subtypep class 'system)
            (error 'non-system-system :name name :class-name (class-name class)))
          (unless (eq (type-of system) class)
            (change-class system class)))
        (parse-component-form
         nil (list*
              :module name
              :pathname (determine-system-directory pathname)
              component-options)))))

  (defmacro defsystem (name &body options)
    `(apply 'register-system-definition ',name ',options)))
;;;; -------------------------------------------------------------------------
;;;; ASDF-Bundle

(uiop/package:define-package :asdf/bundle
  (:recycle :asdf/bundle :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component :asdf/operation
   :asdf/action :asdf/lisp-action :asdf/plan :asdf/operate :asdf/defsystem)
  (:export
   #:bundle-op #:bundle-type #:program-system
   #:bundle-system #:bundle-pathname-type #:bundlable-file-p #:direct-dependency-files
   #:monolithic-op #:monolithic-bundle-op #:operation-monolithic-p
   #:basic-compile-bundle-op #:prepare-bundle-op
   #:compile-bundle-op #:load-bundle-op #:monolithic-compile-bundle-op #:monolithic-load-bundle-op
   #:lib-op #:monolithic-lib-op
   #:dll-op #:monolithic-dll-op
   #:deliver-asd-op #:monolithic-deliver-asd-op
   #:program-op #:image-op #:compiled-file #:precompiled-system #:prebuilt-system
   #:user-system-p #:user-system #:trivial-system-p
   #:make-build
   #:build-args #:name-suffix #:prologue-code #:epilogue-code #:static-library))
(in-package :asdf/bundle)

(with-upgradability ()
  (defclass bundle-op (basic-compile-op)
    ((build-args :initarg :args :initform nil :accessor extra-build-args)
     (name-suffix :initarg :name-suffix :initform nil)
     (bundle-type :initform :no-output-file :reader bundle-type)
     #+(or clasp ecl) (lisp-files :initform nil :accessor extra-object-files)))

  (defclass monolithic-op (operation) ()
    (:documentation "A MONOLITHIC operation operates on a system *and all of its
dependencies*.  So, for example, a monolithic concatenate operation will
concatenate together a system's components and all of its dependencies, but a
simple concatenate operation will concatenate only the components of the system
itself.")) ;; operation on a system and its dependencies

  (defclass monolithic-bundle-op (monolithic-op bundle-op)
    ;; Old style way of specifying prologue and epilogue on ECL: in the monolithic operation
    ((prologue-code :initform nil :accessor prologue-code)
     (epilogue-code :initform nil :accessor epilogue-code)))

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

  (defmethod prologue-code ((x t)) nil)
  (defmethod epilogue-code ((x t)) nil)
  (defmethod no-uiop ((x t)) nil)
  (defmethod prefix-lisp-object-files ((x t)) nil)
  (defmethod postfix-lisp-object-files ((x t)) nil)
  (defmethod extra-object-files ((x t)) nil)
  (defmethod extra-build-args ((x t)) nil)

  (defclass link-op (bundle-op) ()
    (:documentation "Abstract operation for linking files together"))

  (defclass gather-op (bundle-op)
    ((gather-op :initform nil :allocation :class :reader gather-op))
    (:documentation "Abstract operation for gathering many input files from a system"))

  (defun operation-monolithic-p (op)
    (typep op 'monolithic-op))

  (defmethod component-depends-on ((o gather-op) (s system))
    (let* ((mono (operation-monolithic-p o))
           (deps
             (required-components
              s :other-systems mono :component-type (if mono 'system '(not system))
                :goal-operation (find-operation o 'load-op)
                :keep-operation 'compile-op)))
      ;; NB: the explicit make-operation on ECL and MKCL
      ;; ensures that we drop the original-initargs and its magic flags when recursing.
      `((,(make-operation (or (gather-op o) (if mono 'lib-op 'compile-op))) ,@deps)
        ,@(call-next-method))))

  ;; create a single fasl for the entire library
  (defclass basic-compile-bundle-op (bundle-op)
    ((bundle-type :initform :fasl)))

  (defclass prepare-bundle-op (sideway-operation)
    ((sideway-operation
      :initform #+(or clasp ecl mkcl) 'load-bundle-op #-(or clasp ecl mkcl) 'load-op
      :allocation :class)))

  (defclass lib-op (link-op gather-op non-propagating-operation)
    ((bundle-type :initform :lib))
    (:documentation "compile the system and produce linkable (.a) library for it."))

  (defclass compile-bundle-op (basic-compile-bundle-op selfward-operation
                               #+(or clasp ecl mkcl) link-op #-(or clasp ecl) gather-op)
    ((selfward-operation :initform '(prepare-bundle-op #+(or clasp ecl) lib-op)
                         :allocation :class)))

  (defclass load-bundle-op (basic-load-op selfward-operation)
    ((selfward-operation :initform '(prepare-bundle-op compile-bundle-op) :allocation :class)))

  ;; NB: since the monolithic-op's can't be sideway-operation's,
  ;; if we wanted lib-op, dll-op, deliver-asd-op to be sideway-operation's,
  ;; we'd have to have the monolithic-op not inherit from the main op,
  ;; but instead inherit from a basic-FOO-op as with basic-compile-bundle-op above.

  (defclass dll-op (link-op gather-op non-propagating-operation)
    ((bundle-type :initform :dll))
    (:documentation "compile the system and produce dynamic (.so/.dll) library for it."))

  (defclass deliver-asd-op (basic-compile-op selfward-operation)
    ((selfward-operation :initform '(compile-bundle-op #+(or clasp ecl mkcl) lib-op) :allocation :class))
    (:documentation "produce an asd file for delivering the system as a single fasl"))


  (defclass monolithic-deliver-asd-op (monolithic-bundle-op deliver-asd-op)
    ((selfward-operation
      :initform '(monolithic-compile-bundle-op #+(or clasp ecl mkcl) monolithic-lib-op)
      :allocation :class))
    (:documentation "produce fasl and asd files for combined system and dependencies."))

  (defclass monolithic-compile-bundle-op (monolithic-bundle-op basic-compile-bundle-op
                                          #+(or clasp ecl mkcl) link-op gather-op non-propagating-operation)
    ((gather-op :initform #+(or clasp ecl mkcl) 'lib-op #-(or clasp ecl mkcl) 'compile-bundle-op :allocation :class))
    (:documentation "Create a single fasl for the system and its dependencies."))

  (defclass monolithic-load-bundle-op (monolithic-bundle-op load-bundle-op)
    ((selfward-operation :initform 'monolithic-compile-bundle-op :allocation :class))
    (:documentation "Load a single fasl for the system and its dependencies."))

  (defclass monolithic-lib-op (monolithic-bundle-op lib-op non-propagating-operation) ()
    (:documentation "Create a single linkable library for the system and its dependencies."))

  (defclass monolithic-dll-op (monolithic-bundle-op dll-op non-propagating-operation)
    ((bundle-type :initform :dll))
    (:documentation "Create a single dynamic (.so/.dll) library for the system and its dependencies."))

  (defclass image-op (monolithic-bundle-op selfward-operation
                      #+(or clasp ecl mkcl) link-op #+(or clasp ecl mkcl) gather-op)
    ((bundle-type :initform :image)
     (selfward-operation :initform '(#-(or clasp ecl mkcl) load-op) :allocation :class))
    (:documentation "create an image file from the system and its dependencies"))

  (defclass program-op (image-op)
    ((bundle-type :initform :program))
    (:documentation "create an executable file from the system and its dependencies"))

  (defun bundle-pathname-type (bundle-type)
    (etypecase bundle-type
      ((eql :no-output-file) nil) ;; should we error out instead?
      ((or null string) bundle-type)
      ((eql :fasl) #-(or clasp ecl mkcl) (compile-file-type) #+(or clasp ecl mkcl) "fasb")
      #+(or clasp ecl)
      ((member :dll :lib :shared-library :static-library :program :object :program)
       (compile-file-type :type bundle-type))
      ((member :image) #+allegro "dxl" #+(and clisp os-windows) "exe" #-(or allegro (and clisp os-windows)) "image")
      ((member :dll :shared-library) (os-cond ((os-macosx-p) "dylib") ((os-unix-p) "so") ((os-windows-p) "dll")))
      ((member :lib :static-library) (os-cond ((os-unix-p) "a")
                                              ((os-windows-p) (if (featurep '(:or :mingw32 :mingw64)) "a" "lib"))))
      ((eql :program) (os-cond ((os-unix-p) nil) ((os-windows-p) "exe")))))

  (defun bundle-output-files (o c)
    (let ((bundle-type (bundle-type o)))
      (unless (or (eq bundle-type :no-output-file) ;; NIL already means something regarding type.
                  (and (null (input-files o c)) (not (member bundle-type '(:image :program)))))
        (let ((name (or (component-build-pathname c)
                        (format nil "~A~@[~A~]" (component-name c) (slot-value o 'name-suffix))))
              (type (bundle-pathname-type bundle-type)))
          (values (list (subpathname (component-pathname c) name :type type))
                  (eq (type-of o) (component-build-operation c)))))))

  (defmethod output-files ((o bundle-op) (c system))
    (bundle-output-files o c))

  #-(or clasp ecl mkcl)
  (progn
    (defmethod perform ((o image-op) (c system))
      (dump-image (output-file o c) :executable (typep o 'program-op)))
    (defmethod perform :before ((o program-op) (c system))
      (setf *image-entry-point* (ensure-function (component-entry-point c)))))

  (defclass compiled-file (file-component)
    ((type :initform #-(or clasp ecl mkcl) (compile-file-type) #+(or clasp ecl mkcl) "fasb")))

  (defclass precompiled-system (system)
    ((build-pathname :initarg :fasl)))

  (defclass prebuilt-system (system)
    ((build-pathname :initarg :static-library :initarg :lib
                     :accessor prebuilt-system-static-library))))


;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;
(with-upgradability ()
  (defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                         &key (name-suffix nil name-suffix-p)
                                         &allow-other-keys)
    (declare (ignore initargs name-suffix))
    (unless name-suffix-p
      (setf (slot-value instance 'name-suffix)
            (unless (typep instance 'program-op)
              ;; "." is no good separator for Logical Pathnames, so we use "--"
              (if (operation-monolithic-p instance) "--all-systems" #-(or clasp ecl mkcl) "--system"))))
    (when (typep instance 'monolithic-bundle-op)
      (destructuring-bind (&key lisp-files prologue-code epilogue-code
                           &allow-other-keys)
          (operation-original-initargs instance)
        (setf (prologue-code instance) prologue-code
              (epilogue-code instance) epilogue-code)
        #-(or clasp ecl) (assert (null (or lisp-files #-mkcl epilogue-code #-mkcl prologue-code)))
        #+(or clasp ecl) (setf (extra-object-files instance) lisp-files)))
    (setf (extra-build-args instance)
          (remove-plist-keys
           '(:type :monolithic :name-suffix :epilogue-code :prologue-code :lisp-files
             :force :force-not :plan-class) ;; TODO: refactor so we don't mix plan and operation arguments
           (operation-original-initargs instance))))

  (defun bundlable-file-p (pathname)
    (let ((type (pathname-type pathname)))
      (declare (ignorable type))
      (or #+(or clasp ecl) (or (equalp type (compile-file-type :type :object))
                               (equalp type (compile-file-type :type :static-library)))
          #+mkcl (or (equalp type (compile-file-type :fasl-p nil))
                     #+(or unix mingw32 mingw64) (equalp type "a") ;; valid on Unix and MinGW
                     #+(and windows (not (or mingw32 mingw64))) (equalp type "lib"))
          #+(or abcl allegro clisp clozure cmu lispworks sbcl scl xcl) (equalp type (compile-file-type)))))

  (defgeneric* (trivial-system-p) (component))

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
    ;; This file selects output files from direct dependencies;
    ;; your component-depends-on method better gathered the correct dependencies in the correct order.
    (while-collecting (collect)
      (map-direct-dependencies
       t o c #'(lambda (sub-o sub-c)
                 (loop :for f :in (funcall key sub-o sub-c)
                       :when (funcall test f) :do (collect f))))))

  (defmethod input-files ((o gather-op) (c system))
    (unless (eq (bundle-type o) :no-output-file)
      (direct-dependency-files o c :test 'bundlable-file-p :key 'output-files)))

  (defun select-bundle-operation (type &optional monolithic)
    (ecase type
      ((:dll :shared-library)
       (if monolithic 'monolithic-dll-op 'dll-op))
      ((:lib :static-library)
       (if monolithic 'monolithic-lib-op 'lib-op))
      ((:fasl)
       (if monolithic 'monolithic-compile-bundle-op 'compile-bundle-op))
      ((:image)
       'image-op)
      ((:program)
       'program-op)))

  ;; DEPRECATED. This is originally from asdf-ecl.lisp. Does anyone use it?
  (defun make-build (system &rest args &key (monolithic nil) (type :fasl)
                             (move-here nil move-here-p)
                             &allow-other-keys)
    (let* ((operation-name (select-bundle-operation type monolithic))
           (move-here-path (if (and move-here
                                    (typep move-here '(or pathname string)))
                               (ensure-pathname move-here :namestring :lisp :ensure-directory t)
                               (system-relative-pathname system "asdf-output/")))
           (operation (apply #'operate operation-name
                             system
                             (remove-plist-keys '(:monolithic :type :move-here) args)))
           (system (find-system system))
           (files (and system (output-files operation system))))
      (if (or move-here (and (null move-here-p)
                             (member operation-name '(:program :image))))
          (loop :with dest-path = (resolve-symlinks* (ensure-directories-exist move-here-path))
                :for f :in files
                :for new-f = (make-pathname :name (pathname-name f)
                                            :type (pathname-type f)
                                            :defaults dest-path)
                :do (rename-file-overwriting-target f new-f)
                :collect new-f)
          files)))

  ;; DEPRECATED. Does anyone use this?
  (defun bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
    (declare (ignore force verbose version))
    (apply #'operate 'deliver-asd-op system args)))

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

  (defmethod component-depends-on ((o gather-op) (c prebuilt-system))
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
                 (remove-if-not 'builtin-system-p
                                (required-components s :component-type 'system
                                                       :keep-operation 'load-op))
                 (while-collecting (x) ;; resolve the sideway-dependencies of s
                   (map-direct-dependencies
                    t 'load-op s
                    #'(lambda (o c)
                        (when (and (typep o 'load-op) (typep c 'system))
                          (x c)))))))
           (depends-on (mapcar 'coerce-name dependencies)))
      (when (pathname-equal asd (system-source-file s))
        (cerror "overwrite the asd file"
                "~/asdf-action:format-action/ is going to overwrite the system definition file ~S which is probably not what you want; you probably need to tweak your output translations."
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
        (when (or (prologue-code o) (epilogue-code o)
                  (prologue-code c) (epilogue-code c))
          (error "prologue-code and epilogue-code are not supported on ~A"
                 (implementation-type)))
        (with-staging-pathname (output-file)
          (combine-fasls fasl-files output-file)))))

  (defmethod input-files ((o load-op) (s precompiled-system))
    (bundle-output-files (find-operation o 'compile-bundle-op) s))

  (defmethod perform ((o load-op) (s precompiled-system))
    (perform-lisp-load-fasl o s))

  (defmethod component-depends-on ((o load-bundle-op) (s precompiled-system))
    #+xcl (declare (ignorable o))
    `((load-op ,s) ,@(call-next-method))))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+(or clasp ecl mkcl)
(with-upgradability ()
  ;; I think that Juanjo intended for this to be,
  ;; but beware the weird bug in test-xach-update-bug.script,
  ;; and also it makes mkcl fail test-logical-pathname.script,
  ;; and ecl fail test-bundle.script.
  ;;(unless (or #+(or clasp ecl) (use-ecl-byte-compiler-p))
  ;;  (setf *load-system-operation* 'load-bundle-op))

  (defun uiop-library-pathname ()
    #+clasp (probe-file* (compile-file-pathname "sys:uiop" :output-type :object))
    #+ecl (or (probe-file* (compile-file-pathname "sys:uiop" :type :lib)) ;; new style
              (probe-file* (compile-file-pathname "sys:uiop" :type :object))) ;; old style
    #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:contrib;uiop"))

  (defun asdf-library-pathname ()
    #+clasp (probe-file* (compile-file-pathname "sys:asdf" :output-type :object))
    #+ecl (or (probe-file* (compile-file-pathname "sys:asdf" :type :lib)) ;; new style
              (probe-file* (compile-file-pathname "sys:asdf" :type :object))) ;; old style
    #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:contrib;asdf"))

  (defun compiler-library-pathname ()
    #+clasp (compile-file-pathname "sys:cmp" :output-type :lib)
    #+ecl (compile-file-pathname "sys:cmp" :type :lib)
    #+mkcl (make-pathname :type (bundle-pathname-type :lib) :defaults #p"sys:cmp"))

  (defun make-library-system (name pathname)
    (make-instance 'prebuilt-system
                   :name (coerce-name name) :static-library (resolve-symlinks* pathname)))

  (defmethod component-depends-on :around ((o image-op) (c system))
    (destructuring-bind ((lib-op . deps)) (call-next-method)
      (flet ((has-it-p (x) (find x deps :test 'equal :key 'coerce-name)))
        `((,lib-op
           ,@(unless (or (no-uiop c) (has-it-p "cmp"))
               `(,(make-library-system
                   "cmp" (compiler-library-pathname))))
           ,@(unless (or (no-uiop c) (has-it-p "uiop") (has-it-p "asdf"))
               `(cond
                  ((system-source-directory :uiop) `(,(find-system :uiop)))
                  ((system-source-directory :asdf) `(,(find-system :asdf)))
                  (t `(,@(if-let (uiop (uiop-library-pathname))
                           `(,(make-library-system "uiop" uiop)))
                       ,(make-library-system "asdf" (asdf-library-pathname))))))
           ,@deps)))))

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
               :prologue-code (or (prologue-code o) (when programp (prologue-code c)))
               :epilogue-code (or (epilogue-code o) (when programp (epilogue-code c)))
               :build-args (or (extra-build-args o) (when programp (extra-build-args c)))
               :extra-object-files (or (extra-object-files o) (when programp (extra-object-files c)))
               :no-uiop (no-uiop c)
               (when programp `(:entry-point ,(component-entry-point c))))))))

#+(and (not asdf-use-unsafe-mac-bundle-op)
       (or (and clasp ecl darwin)
           (and abcl darwin (not abcl-bundle-op-supported))))
(defmethod perform :before ((o basic-compile-bundle-op) (c component))
  (unless (featurep :asdf-use-unsafe-mac-bundle-op)
    (cerror "Continue after modifying *FEATURES*."
            "BASIC-COMPILE-BUNDLE-OP operations are not supported on Mac OS X for this lisp.~%~T~
To continue, push :asdf-use-unsafe-mac-bundle-op onto *FEATURES*.~%~T~
Please report to ASDF-DEVEL if this works for you.")))


;;; Backward compatibility with pre-3.1.2 names
;; (defclass fasl-op (selfward-operation)
;;   ((selfward-operation :initform 'compile-bundle-op :allocation :class)))
;; (defclass load-fasl-op (selfward-operation)
;;   ((selfward-operation :initform 'load-bundle-op :allocation :class)))
;; (defclass binary-op (selfward-operation)
;;   ((selfward-operation :initform 'deliver-asd-op :allocation :class)))
;; (defclass monolithic-fasl-op (selfward-operation)
;;   ((selfward-operation :initform 'monolithic-compile-bundle-op :allocation :class)))
;; (defclass monolithic-load-fasl-op (selfward-operation)
;;   ((selfward-operation :initform 'monolithic-load-bundle-op :allocation :class)))
;; (defclass monolithic-binary-op (selfward-operation)
;;   ((selfward-operation :initform 'monolithic-deliver-asd-op :allocation :class)))
;;;; -------------------------------------------------------------------------
;;;; Concatenate-source

(uiop/package:define-package :asdf/concatenate-source
  (:recycle :asdf/concatenate-source :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/operation
   :asdf/system :asdf/find-system
   :asdf/action :asdf/lisp-action :asdf/bundle)
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
  (defclass basic-concatenate-source-op (bundle-op)
    ((bundle-type :initform "lisp")))
  (defclass basic-load-concatenated-source-op (basic-load-op selfward-operation) ())
  (defclass basic-compile-concatenated-source-op (basic-compile-op selfward-operation) ())
  (defclass basic-load-compiled-concatenated-source-op (basic-load-op selfward-operation) ())

  (defclass concatenate-source-op (basic-concatenate-source-op non-propagating-operation) ())
  (defclass load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class)))
  (defclass compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op concatenate-source-op) :allocation :class)))
  (defclass load-compiled-concatenated-source-op (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform '(prepare-op compile-concatenated-source-op) :allocation :class)))

  (defclass monolithic-concatenate-source-op (basic-concatenate-source-op monolithic-bundle-op non-propagating-operation) ())
  (defclass monolithic-load-concatenated-source-op (basic-load-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class)))
  (defclass monolithic-compile-concatenated-source-op (basic-compile-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-concatenate-source-op :allocation :class)))
  (defclass monolithic-load-compiled-concatenated-source-op (basic-load-compiled-concatenated-source-op)
    ((selfward-operation :initform 'monolithic-compile-concatenated-source-op :allocation :class)))

  (defmethod input-files ((operation basic-concatenate-source-op) (s system))
    (loop :with encoding = (or (component-encoding s) *default-encoding*)
          :with other-encodings = '()
          :with around-compile = (around-compile-hook s)
          :with other-around-compile = '()
          :for c :in (required-components
                      s :goal-operation 'compile-op
                        :keep-operation 'compile-op
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
        :asdf/defsystem ;; Using the old name of :asdf/parse-defsystem for compatibility
        :asdf/upgrade :asdf/component :asdf/system :asdf/find-system :asdf/lisp-action)
  (:export
   #:package-inferred-system #:sysdef-package-inferred-system-search
   #:package-system ;; backward compatibility only. To be removed.
   #:register-system-packages
   #:*defpackage-forms* #:*package-inferred-systems* #:package-inferred-system-missing-package-error))
(in-package :asdf/package-inferred-system)

(with-upgradability ()
  (defparameter *defpackage-forms* '(defpackage define-package))

  (defun initial-package-inferred-systems-table ()
    (let ((h (make-hash-table :test 'equal)))
      (dolist (p (list-all-packages))
        (dolist (n (package-names p))
          (setf (gethash n h) t)))
      h))

  (defvar *package-inferred-systems* (initial-package-inferred-systems-table))

  (defclass package-inferred-system (system)
    ())

  ;; For backward compatibility only. To be removed in an upcoming release:
  (defclass package-system (package-inferred-system) ())

  (defun defpackage-form-p (form)
    (and (consp form)
         (member (car form) *defpackage-forms*)))

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
    (if-let ((system-name (gethash package-name *package-inferred-systems*)))
      system-name
      (string-downcase package-name)))

  (defun package-inferred-system-file-dependencies (file &optional system)
    (if-let (defpackage-form (file-defpackage-form file))
      (remove t (mapcar 'package-name-system (package-dependencies defpackage-form)))
      (error 'package-inferred-system-missing-package-error :system system :pathname file)))

  (defun same-package-inferred-system-p (system name directory subpath dependencies)
    (and (eq (type-of system) 'package-inferred-system)
         (equal (component-name system) name)
         (pathname-equal directory (component-pathname system))
         (equal dependencies (component-sideway-dependencies system))
         (let ((children (component-children system)))
           (and (length=n-p children 1)
                (let ((child (first children)))
                  (and (eq (type-of child) 'cl-source-file)
                       (equal (component-name child) "lisp")
                       (and (slot-boundp child 'relative-pathname)
                            (equal (slot-value child 'relative-pathname) subpath))))))))

  (defun sysdef-package-inferred-system-search (system)
    (let ((primary (primary-system-name system)))
      (unless (equal primary system)
        (let ((top (find-system primary nil)))
          (when (typep top 'package-inferred-system)
            (if-let (dir (system-source-directory top))
              (let* ((sub (subseq system (1+ (length primary))))
                     (f (probe-file* (subpathname dir sub :type "lisp")
                                     :truename *resolve-symlinks*)))
                (when (file-pathname-p f)
                  (let ((dependencies (package-inferred-system-file-dependencies f system))
                        (previous (cdr (system-registered-p system))))
                    (if (same-package-inferred-system-p previous system dir sub dependencies)
                        previous
                        (eval `(defsystem ,system
                                 :class package-inferred-system
                                 :source-file nil
                                 :pathname ,dir
                                 :depends-on ,dependencies
                                 :components ((cl-source-file "lisp" :pathname ,sub)))))))))))))))

(with-upgradability ()
  (pushnew 'sysdef-package-inferred-system-search *system-definition-search-functions*)
  (setf *system-definition-search-functions*
        (remove (find-symbol* :sysdef-package-system-search :asdf/package-system nil)
                *system-definition-search-functions*)))
;;;; -------------------------------------------------------------------------
;;; Internal hacks for backward-compatibility

(uiop/package:define-package :asdf/backward-internals
  (:recycle :asdf/backward-internals :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/find-system)
  (:export ;; for internal use
   #:make-sub-operation
   #:load-sysdef #:make-temporary-package))
(in-package :asdf/backward-internals)

(when-upgrading (:when (fboundp 'make-sub-operation))
  (defun make-sub-operation (c o dep-c dep-o)
    (declare (ignore c o dep-c dep-o)) (asdf-upgrade-error)))

;;;; load-sysdef
(with-upgradability ()
  (defun load-sysdef (name pathname)
    (load-asd pathname :name name))

  (defun make-temporary-package ()
    ;; For loading a .asd file, we don't make a temporary package anymore,
    ;; but use ASDF-USER. I'd like to have this function do this,
    ;; but since whoever uses it is likely to delete-package the result afterwards,
    ;; this would be a bad idea, so preserve the old behavior.
    (make-package (fresh-package-name :prefix :asdf :index 0) :use '(:cl :asdf))))

;;;; -------------------------------------------------------------------------
;;; Backward-compatible interfaces

(uiop/package:define-package :asdf/backward-interface
  (:recycle :asdf/backward-interface :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade
   :asdf/component :asdf/system :asdf/find-system :asdf/operation :asdf/action
   :asdf/lisp-action :asdf/plan :asdf/operate :asdf/output-translations)
  (:export
   #:*asdf-verbose*
   #:operation-error #:compile-error #:compile-failed #:compile-warned
   #:error-component #:error-operation #:traverse
   #:component-load-dependencies
   #:enable-asdf-binary-locations-compatibility
   #:operation-forced
   #:operation-on-failure #:operation-on-warnings #:on-failure #:on-warnings
   #:component-property
   #:run-shell-command
   #:system-definition-pathname))
(in-package :asdf/backward-interface)

(with-upgradability ()
  (define-condition operation-error (error) ;; Bad, backward-compatible name
    ;; Used by SBCL, cffi-tests, clsql-mysql, clsql-uffi, qt, elephant, uffi-tests, sb-grovel
    ((component :reader error-component :initarg :component)
     (operation :reader error-operation :initarg :operation))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~A while invoking ~A on ~A~@:>")
                       (type-of c) (error-operation c) (error-component c)))))
  (define-condition compile-error (operation-error) ())
  (define-condition compile-failed (compile-error) ())
  (define-condition compile-warned (compile-error) ())

  (defun component-load-dependencies (component)
    ;; Old deprecated name for the same thing. Please update your software.
    (component-sideway-dependencies component))

  (defgeneric operation-forced (operation)) ;; Used by swank.asd for swank-loader.
  (defmethod operation-forced ((o operation)) (getf (operation-original-initargs o) :force))

  (defgeneric operation-on-warnings (operation))
  (defgeneric operation-on-failure (operation))
  (defgeneric (setf operation-on-warnings) (x operation))
  (defgeneric (setf operation-on-failure) (x operation))
  (defmethod operation-on-warnings ((o operation))
    *compile-file-warnings-behaviour*)
  (defmethod operation-on-failure ((o operation))
    *compile-file-failure-behaviour*)
  (defmethod (setf operation-on-warnings) (x (o operation))
    (setf *compile-file-warnings-behaviour* x))
  (defmethod (setf operation-on-failure) (x (o operation))
    (setf *compile-file-failure-behaviour* x))

  (defun system-definition-pathname (x)
    ;; As of 2.014.8, we mean to make this function obsolete,
    ;; but that won't happen until all clients have been updated.
    ;;(cerror "Use ASDF:SYSTEM-SOURCE-FILE instead"
    "Function ASDF:SYSTEM-DEFINITION-PATHNAME is obsolete.
It used to expose ASDF internals with subtle differences with respect to
user expectations, that have been refactored away since.
We recommend you use ASDF:SYSTEM-SOURCE-FILE instead
for a mostly compatible replacement that we're supporting,
or even ASDF:SYSTEM-SOURCE-DIRECTORY or ASDF:SYSTEM-RELATIVE-PATHNAME
if that's whay you mean." ;;)
    (system-source-file x))

  (defgeneric* (traverse) (operation component &key &allow-other-keys)
    (:documentation
     "Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))
  (define-convenience-action-methods traverse (operation component &key))

  (defmethod traverse ((o operation) (c component) &rest keys &key plan-class &allow-other-keys)
    (plan-actions (apply 'make-plan plan-class o c keys))))


;;;; ASDF-Binary-Locations compatibility
;; This remains supported for legacy user, but not recommended for new users.
(with-upgradability ()
  (defun enable-asdf-binary-locations-compatibility
      (&key
       (centralize-lisp-binaries nil)
       (default-toplevel-directory
        (subpathname (user-homedir-pathname) ".fasls/")) ;; Use ".cache/common-lisp/" instead ???
       (include-per-user-information nil)
       (map-all-source-files (or #+(or clasp clisp ecl mkcl) t nil))
       (source-to-target-mappings nil)
       (file-types `(,(compile-file-type)
                     "build-report"
                     #+(or clasp ecl) (compile-file-type :type :object)
                     #+mkcl (compile-file-type :fasl-p nil)
                     #+clisp "lib" #+sbcl "cfasl"
                     #+sbcl "sbcl-warnings" #+clozure "ccl-warnings")))
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


;;; run-shell-command
;; WARNING! The function below is not just deprecated but also dysfunctional.
;; Please use asdf/run-program:run-program instead.
(with-upgradability ()
  (defun run-shell-command (control-string &rest args)
    "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code.

PLEASE DO NOT USE.
Deprecated function, for backward-compatibility only.
Please use UIOP:RUN-PROGRAM instead."
    (let ((command (apply 'format nil control-string args)))
      (asdf-message "; $ ~A~%" command)
      (let ((exit-code
              (ignore-errors
               (nth-value 2 (run-program command :force-shell t :ignore-error-status t
                                                 :output *verbose-out*)))))
        (typecase exit-code
          ((integer 0 255) exit-code)
          (t 255))))))

(with-upgradability ()
  (defvar *asdf-verbose* nil)) ;; backward-compatibility with ASDF2 only. Unused.

;; backward-compatibility methods. Do NOT use in new code. NOT SUPPORTED.
(with-upgradability ()
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
    new-value))
;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.

(uiop/package:define-package :asdf/interface
  (:nicknames :asdf :asdf-utilities)
  (:recycle :asdf/interface :asdf)
  (:unintern
   #:loaded-systems ; makes for annoying SLIME completion
   #:output-files-for-system-and-operation) ; ASDF-BINARY-LOCATION function we use to detect ABL
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/cache
   :asdf/component :asdf/system :asdf/find-system :asdf/find-component
   :asdf/operation :asdf/action :asdf/lisp-action
   :asdf/output-translations :asdf/source-registry
   :asdf/plan :asdf/operate :asdf/parse-defsystem :asdf/bundle :asdf/concatenate-source
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
   #:prepare-source-op #:load-source-op #:test-op
   #:feature #:version #:version-satisfies #:upgrade-asdf
   #:implementation-identifier #:implementation-type #:hostname
   #:input-files #:output-files #:output-file #:perform #:perform-with-restarts
   #:operation-done-p #:explain #:action-description #:component-sideway-dependencies
   #:needed-in-image-p
   #:component-load-dependencies #:run-shell-command ; deprecated, do not use
   #:bundle-op #:monolithic-bundle-op #:precompiled-system #:compiled-file #:bundle-system
   #:program-system #:make-build
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
   #:package-system ;; backward-compatibility during migration, to be removed in a further release.

   #:component-children          ; component accessors
   #:component-children-by-name
   #:component-pathname
   #:component-relative-pathname
   #:component-name
   #:component-version
   #:component-parent
   #:component-system
   #:component-encoding
   #:component-external-format

   #:component-depends-on ; backward-compatible name rather than action-depends-on
   #:module-components ; backward-compatibility
   #:operation-on-warnings #:operation-on-failure ; backward-compatibility
   #:component-property ; backward-compatibility
   #:traverse ; backward-compatibility

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
   #:*load-system-operation*
   #:*asdf-verbose* ;; unused. For backward-compatibility only.
   #:*verbose-out*

   #:asdf-version

   #:compile-condition #:compile-file-error #:compile-warned-error #:compile-failed-error
   #:compile-warned-warning #:compile-failed-warning
   #:operation-error #:compile-failed #:compile-warned #:compile-error ;; backward compatibility
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
   #:duplicate-names #:non-toplevel-system #:non-system-system
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
   #:system-registered-p #:registered-systems #:already-loaded-systems
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
   ))

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
        :asdf/upgrade :asdf/find-system :asdf/operate :asdf/bundle))
(in-package :asdf/footer)

;;;; Hook ASDF into the implementation's REQUIRE and other entry points.
#+(or abcl clasp clisp clozure cmu ecl mkcl sbcl)
(with-upgradability ()
  (if-let (x (and #+clisp (find-symbol* '#:*module-provider-functions* :custom nil)))
    (eval `(pushnew 'module-provide-asdf
                    #+abcl sys::*module-provider-functions*
                    #+(or clasp cmu ecl) ext:*module-provider-functions*
                    #+clisp ,x
                    #+clozure ccl:*module-provider-functions*
                    #+mkcl mk-ext:*module-provider-functions*
                    #+sbcl sb-ext:*module-provider-functions*)))

  #+(or clasp ecl mkcl)
  (progn
    (pushnew '("fasb" . si::load-binary) si::*load-hooks* :test 'equal :key 'car)

    #+(or (and clasp windows) (and ecl win32) (and mkcl windows))
    (unless (assoc "asd" #+(or clasp ecl) ext:*load-hooks* #+mkcl si::*load-hooks* :test 'equal)
      (appendf #+(or clasp ecl) ext:*load-hooks* #+mkcl si::*load-hooks* '(("asd" . si::load-source))))

    (setf #+(or clasp ecl) ext:*module-provider-functions* #+mkcl mk-ext::*module-provider-functions*
          (loop :for f :in #+(or clasp ecl) ext:*module-provider-functions*
                #+mkcl mk-ext::*module-provider-functions*
                :collect
                (if (eq f 'module-provide-asdf) f
                    #'(lambda (name)
                        (let ((l (multiple-value-list (funcall f name))))
                          (and (first l) (register-preloaded-system (coerce-name name)))
                          (values-list l))))))))

#+cmu ;; Hook into the CMUCL herald.
(with-upgradability ()
  (defun herald-asdf (stream)
    (format stream "    ASDF ~A" (asdf-version)))
  (setf (getf ext:*herald-items* :asdf) `(herald-asdf)))


;;;; Done!
(with-upgradability ()
  #+allegro
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* asdf/common-lisp::*acl-warn-save*))

  (dolist (f '(:asdf :asdf2 :asdf3 :asdf3.1 :asdf-package-system)) (pushnew f *features*))

  ;; Provide both lowercase and uppercase, to satisfy more people, especially LispWorks users.
  (provide "asdf") (provide "ASDF")

  (cleanup-upgraded-asdf))

(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))
