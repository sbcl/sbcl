;;; -*- mode: common-lisp; package: asdf; -*-
;;; This is ASDF: Another System Definition Facility.
;;;
;;; Feedback, bug reports, and patches are all welcome:
;;; please mail to <asdf-devel@common-lisp.net>.
;;; Note first that the canonical source for ASDF is presently
;;; <URL:http://common-lisp.net/project/asdf/>.
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the git HEAD
;;; is the latest development version, whereas the revision tagged
;;; RELEASE may be slightly older but is considered `stable'

;;; -- LICENSE START
;;; (This is the MIT / X Consortium license as taken from
;;;  http://www.opensource.org/licenses/mit-license.html on or about
;;;  Monday; July 13, 2009)
;;;
;;; Copyright (c) 2001-2010 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; -- LICENSE END

;;; The problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file.

#+xcvb (module ())

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; make package if it doesn't exist yet.
  ;;; DEFPACKAGE may cause errors on discrepancies, so we avoid it.
  (unless (find-package :asdf)
    (make-package :asdf :use '(:cl)))
  ;;; Implementation-dependent tweaks
  ;; (declaim (optimize (speed 2) (debug 2) (safety 3))) ; NO: rely on the implementation defaults.
  #+allegro
  (setf excl::*autoload-package-name-alist*
        (remove "asdf" excl::*autoload-package-name-alist*
                :test 'equalp :key 'car))
  #+ecl (require :cmp))

(in-package :asdf)

;;;; Create packages in a way that is compatible with hot-upgrade.
;;;; See https://bugs.launchpad.net/asdf/+bug/485687
;;;; See more at the end of the file.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *asdf-version* nil)
  (defvar *upgraded-p* nil)
  (let* ((asdf-version "2.010") ;; same as 2.146
         (existing-asdf (fboundp 'find-system))
         (existing-version *asdf-version*)
         (already-there (equal asdf-version existing-version)))
    (unless (and existing-asdf already-there)
      (when existing-asdf
        (format *error-output*
                "~&Upgrading ASDF package ~@[from version ~A ~]to version ~A~%"
                existing-version asdf-version))
      (labels
          ((unlink-package (package)
             (let ((u (find-package package)))
               (when u
                 (ensure-unintern u
                   (loop :for s :being :each :present-symbol :in u :collect s))
                 (loop :for p :in (package-used-by-list u) :do
                   (unuse-package u p))
                 (delete-package u))))
           (ensure-exists (name nicknames use)
             (let ((previous
                    (remove-duplicates
                     (mapcar #'find-package (cons name nicknames))
                     :from-end t)))
               ;; do away with packages with conflicting (nick)names
               (map () #'unlink-package (cdr previous))
               ;; reuse previous package with same name
               (let ((p (car previous)))
                 (cond
                   (p
                    (rename-package p name nicknames)
                    (ensure-use p use)
                    p)
                   (t
                    (make-package name :nicknames nicknames :use use))))))
           (find-sym (symbol package)
             (find-symbol (string symbol) package))
           (intern* (symbol package)
             (intern (string symbol) package))
           (remove-symbol (symbol package)
             (let ((sym (find-sym symbol package)))
               (when sym
                 (unexport sym package)
                 (unintern sym package)
                 sym)))
           (ensure-unintern (package symbols)
             (loop :with packages = (list-all-packages)
               :for sym :in symbols
               :for removed = (remove-symbol sym package)
               :when removed :do
               (loop :for p :in packages :do
                 (when (eq removed (find-sym sym p))
                   (unintern removed p)))))
           (ensure-shadow (package symbols)
             (shadow symbols package))
           (ensure-use (package use)
             (dolist (used (reverse use))
               (do-external-symbols (sym used)
                 (unless (eq sym (find-sym sym package))
                   (remove-symbol sym package)))
               (use-package used package)))
           (ensure-fmakunbound (package symbols)
             (loop :for name :in symbols
               :for sym = (find-sym name package)
               :when sym :do (fmakunbound sym)))
           (ensure-export (package export)
             (let ((formerly-exported-symbols nil)
                   (bothly-exported-symbols nil)
                   (newly-exported-symbols nil))
               (loop :for sym :being :each :external-symbol :in package :do
                 (if (member sym export :test 'string-equal)
                     (push sym bothly-exported-symbols)
                     (push sym formerly-exported-symbols)))
               (loop :for sym :in export :do
                 (unless (member sym bothly-exported-symbols :test 'string-equal)
                   (push sym newly-exported-symbols)))
               (loop :for user :in (package-used-by-list package)
                 :for shadowing = (package-shadowing-symbols user) :do
                 (loop :for new :in newly-exported-symbols
                   :for old = (find-sym new user)
                   :when (and old (not (member old shadowing)))
                   :do (unintern old user)))
               (loop :for x :in newly-exported-symbols :do
                 (export (intern* x package)))))
           (ensure-package (name &key nicknames use unintern fmakunbound shadow export)
             (let* ((p (ensure-exists name nicknames use)))
               (ensure-unintern p unintern)
               (ensure-shadow p shadow)
               (ensure-export p export)
               (ensure-fmakunbound p fmakunbound)
               p)))
        (macrolet
            ((pkgdcl (name &key nicknames use export
                           redefined-functions unintern fmakunbound shadow)
                 `(ensure-package
                   ',name :nicknames ',nicknames :use ',use :export ',export
                   :shadow ',shadow
                   :unintern ',(append #-(or gcl ecl) redefined-functions unintern)
                   :fmakunbound ',(append fmakunbound))))
          (pkgdcl
           :asdf
           :nicknames (:asdf-utilities) ;; DEPRECATED! Do not use, for backward compatibility only.
           :use (:common-lisp)
           :redefined-functions
           (#:perform #:explain #:output-files #:operation-done-p
            #:perform-with-restarts #:component-relative-pathname
            #:system-source-file #:operate #:find-component #:find-system
            #:apply-output-translations #:translate-pathname* #:resolve-location)
           :unintern
           (#:*asdf-revision* #:around #:asdf-method-combination
            #:split #:make-collector)
           :fmakunbound
           (#:system-source-file
            #:component-relative-pathname #:system-relative-pathname
            #:process-source-registry
            #:inherit-source-registry #:process-source-registry-directive)
           :export
           (#:defsystem #:oos #:operate #:find-system #:run-shell-command
            #:system-definition-pathname #:find-component ; miscellaneous
            #:compile-system #:load-system #:test-system #:clear-system
            #:compile-op #:load-op #:load-source-op
            #:test-op
            #:operation               ; operations
            #:feature                 ; sort-of operation
            #:version                 ; metaphorically sort-of an operation
            #:version-satisfies

            #:input-files #:output-files #:output-file #:perform ; operation methods
            #:operation-done-p #:explain

            #:component #:source-file
            #:c-source-file #:cl-source-file #:java-source-file
            #:static-file
            #:doc-file
            #:html-file
            #:text-file
            #:source-file-type
            #:module                     ; components
            #:system
            #:unix-dso

            #:module-components          ; component accessors
            #:module-components-by-name  ; component accessors
            #:component-pathname
            #:component-relative-pathname
            #:component-name
            #:component-version
            #:component-parent
            #:component-property
            #:component-system

            #:component-depends-on

            #:system-description
            #:system-long-description
            #:system-author
            #:system-maintainer
            #:system-license
            #:system-licence
            #:system-source-file
            #:system-source-directory
            #:system-relative-pathname
            #:map-systems

            #:operation-on-warnings
            #:operation-on-failure
            #:component-visited-p
            ;;#:*component-parent-pathname*
            #:*system-definition-search-functions*
            #:*central-registry*         ; variables
            #:*compile-file-warnings-behaviour*
            #:*compile-file-failure-behaviour*
            #:*resolve-symlinks*
            #:*asdf-verbose*

            #:asdf-version

            #:operation-error #:compile-failed #:compile-warned #:compile-error
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
            #:duplicate-names

            #:try-recompiling
            #:retry
            #:accept                     ; restarts
            #:coerce-entry-to-directory
            #:remove-entry-from-registry

            #:clear-configuration
            #:initialize-output-translations
            #:disable-output-translations
            #:clear-output-translations
            #:ensure-output-translations
            #:apply-output-translations
            #:compile-file*
            #:compile-file-pathname*
            #:enable-asdf-binary-locations-compatibility
            #:*default-source-registries*
            #:initialize-source-registry
            #:compute-source-registry
            #:clear-source-registry
            #:ensure-source-registry
            #:process-source-registry
            #:system-registered-p
            #:asdf-message

            ;; Utilities
            #:absolute-pathname-p
	    ;; #:aif #:it
            ;; #:appendf
            #:coerce-name
            #:directory-pathname-p
            ;; #:ends-with
            #:ensure-directory-pathname
            #:getenv
            ;; #:get-uid
            ;; #:length=n-p
            #:merge-pathnames*
            #:pathname-directory-pathname
            #:read-file-forms
	    ;; #:remove-keys
	    ;; #:remove-keyword
            #:resolve-symlinks
            #:split-string
            #:component-name-to-pathname-components
            #:split-name-type
            #:subdirectories
            #:truenamize
            #:while-collecting)))
        (setf *asdf-version* asdf-version
              *upgraded-p* (if existing-version
                               (cons existing-version *upgraded-p*)
                               *upgraded-p*))))))

;; More cleanups in case of hot-upgrade. See https://bugs.launchpad.net/asdf/+bug/485687
(when *upgraded-p*
   #+ecl
   (when (find-class 'compile-op nil)
     (defmethod update-instance-for-redefined-class :after
         ((c compile-op) added deleted plist &key)
       (declare (ignore added deleted))
       (let ((system-p (getf plist 'system-p)))
         (when system-p (setf (getf (slot-value c 'flags) :system-p) system-p)))))
   (when (find-class 'module nil)
     (eval
      '(progn
         (defmethod update-instance-for-redefined-class :after
             ((m module) added deleted plist &key)
           (declare (ignorable deleted plist))
           (when *asdf-verbose* (format *trace-output* "Updating ~A~%" m))
           (when (member 'components-by-name added)
             (compute-module-components-by-name m)))
         (defmethod update-instance-for-redefined-class :after
             ((s system) added deleted plist &key)
           (declare (ignorable deleted plist))
           (when *asdf-verbose* (format *trace-output* "Updating ~A~%" s))
           (when (member 'source-file added)
             (%set-system-source-file (probe-asd (component-name s) (component-pathname s)) s)))))))

;;;; -------------------------------------------------------------------------
;;;; User-visible parameters
;;;;
(defun asdf-version ()
  "Exported interface to the version of ASDF currently installed. A string.
You can compare this string with e.g.:
(ASDF:VERSION-SATISFIES (ASDF:ASDF-VERSION) \"2.000\")."
  *asdf-version*)

(defvar *resolve-symlinks* t
  "Determine whether or not ASDF resolves symlinks when defining systems.

Defaults to T.")

(defvar *compile-file-warnings-behaviour*
  (or #+clisp :ignore :warn)
  "How should ASDF react if it encounters a warning when compiling a file?
Valid values are :error, :warn, and :ignore.")

(defvar *compile-file-failure-behaviour*
  (or #+sbcl :error #+clisp :ignore :warn)
  "How should ASDF react if it encounters a failure (per the ANSI spec of COMPILE-FILE)
when compiling a file?  Valid values are :error, :warn, and :ignore.
Note that ASDF ALWAYS raises an error if it fails to create an output file when compiling.")

(defvar *verbose-out* nil)

(defvar *asdf-verbose* t)

(defparameter +asdf-methods+
  '(perform-with-restarts perform explain output-files operation-done-p))

#+allegro
(eval-when (:compile-toplevel :execute)
  (defparameter *acl-warn-save*
                (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
                  excl:*warn-on-nested-reader-conditionals*))
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* nil)))

;;;; -------------------------------------------------------------------------
;;;; ASDF Interface, in terms of generic functions.
(macrolet
    ((defdef (def* def)
       `(defmacro ,def* (name formals &rest rest)
          `(progn
             #+(or ecl gcl) (fmakunbound ',name)
             ,(when (and #+ecl (symbolp name))
                `(declaim (notinline ,name))) ; fails for setf functions on ecl
             (,',def ,name ,formals ,@rest)))))
  (defdef defgeneric* defgeneric)
  (defdef defun* defun))

(defgeneric* find-system (system &optional error-p))
(defgeneric* perform-with-restarts (operation component))
(defgeneric* perform (operation component))
(defgeneric* operation-done-p (operation component))
(defgeneric* explain (operation component))
(defgeneric* output-files (operation component))
(defgeneric* input-files (operation component))
(defgeneric* component-operation-time (operation component))
(defgeneric* operation-description (operation component)
  (:documentation "returns a phrase that describes performing this operation
on this component, e.g. \"loading /a/b/c\".
You can put together sentences using this phrase."))

(defgeneric* system-source-file (system)
  (:documentation "Return the source file in which system is defined."))

(defgeneric* component-system (component)
  (:documentation "Find the top-level system containing COMPONENT"))

(defgeneric* component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))

(defgeneric* component-relative-pathname (component)
  (:documentation "Returns a pathname for the component argument intended to be
interpreted relative to the pathname of that component's parent.
Despite the function's name, the return value may be an absolute
pathname, because an absolute pathname may be interpreted relative to
another pathname in a degenerate way."))

(defgeneric* component-property (component property))

(defgeneric* (setf component-property) (new-value component property))

(defgeneric* version-satisfies (component version))

(defgeneric* find-component (base path)
  (:documentation "Finds the component with PATH starting from BASE module;
if BASE is nil, then the component is assumed to be a system."))

(defgeneric* source-file-type (component system))

(defgeneric* operation-ancestor (operation)
  (:documentation
   "Recursively chase the operation's parent pointer until we get to
the head of the tree"))

(defgeneric* component-visited-p (operation component)
  (:documentation "Returns the value stored by a call to
VISIT-COMPONENT, if that has been called, otherwise NIL.
This value stored will be a cons cell, the first element
of which is a computed key, so not interesting.  The
CDR wil be the DATA value stored by VISIT-COMPONENT; recover
it as (cdr (component-visited-p op c)).
  In the current form of ASDF, the DATA value retrieved is
effectively a boolean, indicating whether some operations are
to be performed in order to do OPERATION X COMPONENT.  If the
data value is NIL, the combination had been explored, but no
operations needed to be performed."))

(defgeneric* visit-component (operation component data)
  (:documentation "Record DATA as being associated with OPERATION
and COMPONENT.  This is a side-effecting function:  the association
will be recorded on the ROOT OPERATION \(OPERATION-ANCESTOR of the
OPERATION\).
  No evidence that DATA is ever interesting, beyond just being
non-NIL.  Using the data field is probably very risky; if there is
already a record for OPERATION X COMPONENT, DATA will be quietly
discarded instead of recorded.
  Starting with 2.006, TRAVERSE will store an integer in data,
so that nodes can be sorted in decreasing order of traversal."))


(defgeneric* (setf visiting-component) (new-value operation component))

(defgeneric* component-visiting-p (operation component))

(defgeneric* component-depends-on (operation component)
  (:documentation
   "Returns a list of dependencies needed by the component to perform
    the operation.  A dependency has one of the following forms:

      (<operation> <component>*), where <operation> is a class
        designator and each <component> is a component
        designator, which means that the component depends on
        <operation> having been performed on each <component>; or

      (FEATURE <feature>), which means that the component depends
        on <feature>'s presence in *FEATURES*.

    Methods specialized on subclasses of existing component types
    should usually append the results of CALL-NEXT-METHOD to the
    list."))

(defgeneric* component-self-dependencies (operation component))

(defgeneric* traverse (operation component)
  (:documentation
"Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))


;;;; -------------------------------------------------------------------------
;;;; General Purpose Utilities

(defmacro while-collecting ((&rest collectors) &body body)
  "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(while-collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
  (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
        (initial-values (mapcar (constantly nil) collectors)))
    `(let ,(mapcar #'list vars initial-values)
       (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
         ,@body
         (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun* pathname-directory-pathname (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
  (when pathname
    (make-pathname :name nil :type nil :version nil :defaults pathname)))

(defun* merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
  "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that if the SPECIFIED pathname
does not have an absolute directory, then the HOST and DEVICE come from the DEFAULTS.
Also, if either argument is NIL, then the other argument is returned unmodified."
  (when (null specified) (return-from merge-pathnames* defaults))
  (when (null defaults) (return-from merge-pathnames* specified))
  (let* ((specified (pathname specified))
         (defaults (pathname defaults))
         (directory (pathname-directory specified))
         (directory
          (cond
            #-(or sbcl cmu)
            ((stringp directory) `(:absolute ,directory) directory)
            #+gcl
            ((and (consp directory) (stringp (first directory)))
             `(:absolute ,@directory))
            ((or (null directory)
                 (and (consp directory) (member (first directory) '(:absolute :relative))))
             directory)
            (t
             (error "Unrecognized directory component ~S in pathname ~S" directory specified))))
         (name (or (pathname-name specified) (pathname-name defaults)))
         (type (or (pathname-type specified) (pathname-type defaults)))
         (version (or (pathname-version specified) (pathname-version defaults))))
    (labels ((ununspecific (x)
               (if (eq x :unspecific) nil x))
             (unspecific-handler (p)
               (if (typep p 'logical-pathname) #'ununspecific #'identity)))
      (multiple-value-bind (host device directory unspecific-handler)
          (ecase (first directory)
            ((nil)
             (values (pathname-host defaults)
                     (pathname-device defaults)
                     (pathname-directory defaults)
                     (unspecific-handler defaults)))
            ((:absolute)
             (values (pathname-host specified)
                     (pathname-device specified)
                     directory
                     (unspecific-handler specified)))
            ((:relative)
             (values (pathname-host defaults)
                     (pathname-device defaults)
                     (if (pathname-directory defaults)
                         (append (pathname-directory defaults) (cdr directory))
                         directory)
                     (unspecific-handler defaults))))
        (make-pathname :host host :device device :directory directory
                       :name (funcall unspecific-handler name)
                       :type (funcall unspecific-handler type)
                       :version (funcall unspecific-handler version))))))

(define-modify-macro appendf (&rest args)
  append "Append onto list") ;; only to be used on short lists.

(define-modify-macro orf (&rest args)
  or "or a flag")

(defun* first-char (s)
  (and (stringp s) (plusp (length s)) (char s 0)))

(defun* last-char (s)
  (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

(defun* asdf-message (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (apply #'format *verbose-out* format-string format-args))

(defun* split-string (string &key max (separator '(#\Space #\Tab)))
  "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
  (block nil
    (let ((list nil) (words 0) (end (length string)))
      (flet ((separatorp (char) (find char separator))
             (done () (return (cons (subseq string 0 end) list))))
        (loop
          :for start = (if (and max (>= words (1- max)))
                           (done)
                           (position-if #'separatorp string :end end :from-end t)) :do
          (when (null start)
            (done))
          (push (subseq string (1+ start) end) list)
          (incf words)
          (setf end start))))))

(defun* split-name-type (filename)
  (let ((unspecific
         ;; Giving :unspecific as argument to make-pathname is not portable.
         ;; See CLHS make-pathname and 19.2.2.2.3.
         ;; We only use it on implementations that support it.
         (or #+(or ccl ecl gcl lispworks sbcl) :unspecific)))
    (destructuring-bind (name &optional (type unspecific))
        (split-string filename :max 2 :separator ".")
      (if (equal name "")
          (values filename unspecific)
          (values name type)))))

(defun* component-name-to-pathname-components (s &key force-directory force-relative)
  "Splits the path string S, returning three values:
A flag that is either :absolute or :relative, indicating
   how the rest of the values are to be interpreted.
A directory path --- a list of strings, suitable for
   use with MAKE-PATHNAME when prepended with the flag
   value.
A filename with type extension, possibly NIL in the
   case of a directory pathname.
FORCE-DIRECTORY forces S to be interpreted as a directory
pathname \(third return value will be NIL, final component
of S will be treated as part of the directory path.

The intention of this function is to support structured component names,
e.g., \(:file \"foo/bar\"\), which will be unpacked to relative
pathnames."
  (check-type s string)
  (when (find #\: s)
    (error "a portable ASDF pathname designator cannot include a #\: character: ~S" s))
  (let* ((components (split-string s :separator "/"))
         (last-comp (car (last components))))
    (multiple-value-bind (relative components)
        (if (equal (first components) "")
            (if (equal (first-char s) #\/)
                (progn
                  (when force-relative
                    (error "absolute pathname designator not allowed: ~S" s))
                  (values :absolute (cdr components)))
                (values :relative nil))
          (values :relative components))
      (setf components (remove "" components :test #'equal))
      (cond
        ((equal last-comp "")
         (values relative components nil)) ; "" already removed
        (force-directory
         (values relative components nil))
        (t
         (values relative (butlast components) last-comp))))))

(defun* remove-keys (key-names args)
  (loop :for (name val) :on args :by #'cddr
    :unless (member (symbol-name name) key-names
                    :key #'symbol-name :test 'equal)
    :append (list name val)))

(defun* remove-keyword (key args)
  (loop :for (k v) :on args :by #'cddr
    :unless (eq k key)
    :append (list k v)))

(defun* getenv (x)
  (#+abcl ext:getenv
   #+allegro sys:getenv
   #+clisp ext:getenv
   #+clozure ccl:getenv
   #+(or cmu scl) (lambda (x) (cdr (assoc x ext:*environment-list* :test #'string=)))
   #+ecl si:getenv
   #+gcl system:getenv
   #+lispworks lispworks:environment-variable
   #+sbcl sb-ext:posix-getenv
   x))

(defun* directory-pathname-p (pathname)
  "Does PATHNAME represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be NIL,
:UNSPECIFIC or the empty string.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing directory."
  (when pathname
    (let ((pathname (pathname pathname)))
      (flet ((check-one (x)
               (member x '(nil :unspecific "") :test 'equal)))
        (and (not (wild-pathname-p pathname))
             (check-one (pathname-name pathname))
             (check-one (pathname-type pathname))
             t)))))

(defun* ensure-directory-pathname (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory form."
  (cond
   ((stringp pathspec)
    (ensure-directory-pathname (pathname pathspec)))
   ((not (pathnamep pathspec))
    (error "Invalid pathname designator ~S" pathspec))
   ((wild-pathname-p pathspec)
    (error "Can't reliably convert wild pathname ~S" pathspec))
   ((directory-pathname-p pathspec)
    pathspec)
   (t
    (make-pathname :directory (append (or (pathname-directory pathspec)
                                          (list :relative))
                                      (list (file-namestring pathspec)))
                   :name nil :type nil :version nil
                   :defaults pathspec))))

(defun* absolute-pathname-p (pathspec)
  (and pathspec (eq :absolute (car (pathname-directory (pathname pathspec))))))

(defun* length=n-p (x n) ;is it that (= (length x) n) ?
  (check-type n (integer 0 *))
  (loop
    :for l = x :then (cdr l)
    :for i :downfrom n :do
    (cond
      ((zerop i) (return (null l)))
      ((not (consp l)) (return nil)))))

(defun* ends-with (s suffix)
  (check-type s string)
  (check-type suffix string)
  (let ((start (- (length s) (length suffix))))
    (and (<= 0 start)
         (string-equal s suffix :start1 start))))

(defun* read-file-forms (file)
  (with-open-file (in file)
    (loop :with eof = (list nil)
     :for form = (read in nil eof)
     :until (eq form eof)
     :collect form)))

#-(and (or win32 windows mswindows mingw32) (not cygwin))
(progn
  #+ecl #.(cl:and (cl:< ext:+ecl-version-number+ 100601)
                  '(ffi:clines "#include <sys/types.h>" "#include <unistd.h>"))
  (defun* get-uid ()
    #+allegro (excl.osi:getuid)
    #+clisp (loop :for s :in '("posix:uid" "LINUX:getuid")
	          :for f = (ignore-errors (read-from-string s))
                  :when f :return (funcall f))
    #+(or cmu scl) (unix:unix-getuid)
    #+ecl #.(cl:if (cl:< ext:+ecl-version-number+ 100601)
                   '(ffi:c-inline () () :int "getuid()" :one-liner t)
                   '(ext::getuid))
    #+sbcl (sb-unix:unix-getuid)
    #-(or allegro clisp cmu ecl sbcl scl)
    (let ((uid-string
           (with-output-to-string (*verbose-out*)
             (run-shell-command "id -ur"))))
      (with-input-from-string (stream uid-string)
        (read-line stream)
        (handler-case (parse-integer (read-line stream))
          (error () (error "Unable to find out user ID")))))))

(defun* pathname-root (pathname)
  (make-pathname :host (pathname-host pathname)
                 :device (pathname-device pathname)
                 :directory '(:absolute)
                 :name nil :type nil :version nil))

(defun* probe-file* (p)
  "when given a pathname P, probes the filesystem for a file or directory
with given pathname and if it exists return its truename."
  (etypecase p
   (null nil)
   (string (probe-file* (parse-namestring p)))
   (pathname (unless (wild-pathname-p p)
               #.(or #+(or allegro clozure cmu ecl sbcl scl) '(probe-file p)
               #+clisp (aif (find-symbol (string :probe-pathname) :ext) `(ignore-errors (,it p)))
	       '(ignore-errors (truename p)))))))

(defun* truenamize (p)
  "Resolve as much of a pathname as possible"
  (block nil
    (when (typep p 'logical-pathname) (return p))
    (let* ((p (merge-pathnames* p))
           (directory (pathname-directory p)))
      (when (typep p 'logical-pathname) (return p))
      (let ((found (probe-file* p)))
        (when found (return found)))
      #-(or sbcl cmu) (when (stringp directory) (return p))
      (when (not (eq :absolute (car directory))) (return p))
      (let ((sofar (probe-file* (pathname-root p))))
        (unless sofar (return p))
        (flet ((solution (directories)
                 (merge-pathnames*
                  (make-pathname :host nil :device nil
                                 :directory `(:relative ,@directories)
                                 :name (pathname-name p)
                                 :type (pathname-type p)
                                 :version (pathname-version p))
                  sofar)))
          (loop :for component :in (cdr directory)
            :for rest :on (cdr directory)
            :for more = (probe-file*
                         (merge-pathnames*
                          (make-pathname :directory `(:relative ,component))
                          sofar)) :do
            (if more
                (setf sofar more)
                (return (solution rest)))
            :finally
            (return (solution nil))))))))

(defun* resolve-symlinks (path)
  #-allegro (truenamize path)
  #+allegro (excl:pathname-resolve-symbolic-links path))

(defun* default-directory ()
  (truenamize (pathname-directory-pathname *default-pathname-defaults*)))

(defun* lispize-pathname (input-file)
  (make-pathname :type "lisp" :defaults input-file))

(defparameter *wild-path*
  (make-pathname :directory '(:relative :wild-inferiors)
                 :name :wild :type :wild :version :wild))

(defun* wilden (path)
  (merge-pathnames* *wild-path* path))

(defun* directorize-pathname-host-device (pathname)
  (let* ((root (pathname-root pathname))
         (wild-root (wilden root))
         (absolute-pathname (merge-pathnames* pathname root))
         (foo (make-pathname :directory '(:absolute "FOO") :defaults root))
         (separator (last-char (namestring foo)))
         (root-namestring (namestring root))
         (root-string
          (substitute-if #\/
                         (lambda (x) (or (eql x #\:)
                                         (eql x separator)))
                         root-namestring)))
    (multiple-value-bind (relative path filename)
        (component-name-to-pathname-components root-string :force-directory t)
      (declare (ignore relative filename))
      (let ((new-base
             (make-pathname :defaults root
                            :directory `(:absolute ,@path))))
        (translate-pathname absolute-pathname wild-root (wilden new-base))))))

;;;; -------------------------------------------------------------------------
;;;; Classes, Conditions

(define-condition system-definition-error (error) ()
  ;; [this use of :report should be redundant, but unfortunately it's not.
  ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
  ;; over print-object; this is always conditions::%print-condition for
  ;; condition objects, which in turn does inheritance of :report options at
  ;; run-time.  fortunately, inheritance means we only need this kludge here in
  ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
  #+cmu (:report print-object))

(declaim (ftype (function (t) t)
                format-arguments format-control
                error-name error-pathname error-condition
                duplicate-names-name
                error-component error-operation
                module-components module-components-by-name
                circular-dependency-components)
         (ftype (function (t t) t) (setf module-components-by-name)))


(define-condition formatted-system-definition-error (system-definition-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
             (apply #'format s (format-control c) (format-arguments c)))))

(define-condition load-system-definition-error (system-definition-error)
  ((name :initarg :name :reader error-name)
   (pathname :initarg :pathname :reader error-pathname)
   (condition :initarg :condition :reader error-condition))
  (:report (lambda (c s)
             (format s "~@<Error while trying to load definition for system ~A from pathname ~A: ~A~@:>"
                     (error-name c) (error-pathname c) (error-condition c)))))

(define-condition circular-dependency (system-definition-error)
  ((components :initarg :components :reader circular-dependency-components))
  (:report (lambda (c s)
             (format s "~@<Circular dependency: ~S~@:>" (circular-dependency-components c)))))

(define-condition duplicate-names (system-definition-error)
  ((name :initarg :name :reader duplicate-names-name))
  (:report (lambda (c s)
             (format s "~@<Error while defining system: multiple components are given same name ~A~@:>"
                     (duplicate-names-name c)))))

(define-condition missing-component (system-definition-error)
  ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
   (parent :initform nil :reader missing-parent :initarg :parent)))

(define-condition missing-component-of-version (missing-component)
  ((version :initform nil :reader missing-version :initarg :version)))

(define-condition missing-dependency (missing-component)
  ((required-by :initarg :required-by :reader missing-required-by)))

(define-condition missing-dependency-of-version (missing-dependency
                                                 missing-component-of-version)
  ())

(define-condition operation-error (error)
  ((component :reader error-component :initarg :component)
   (operation :reader error-operation :initarg :operation))
  (:report (lambda (c s)
             (format s "~@<erred while invoking ~A on ~A~@:>"
                     (error-operation c) (error-component c)))))
(define-condition compile-error (operation-error) ())
(define-condition compile-failed (compile-error) ())
(define-condition compile-warned (compile-error) ())

(defclass component ()
  ((name :accessor component-name :initarg :name :documentation
         "Component name: designator for a string composed of portable pathname characters")
   (version :accessor component-version :initarg :version)
   ;; This one is used by POIU. Maybe in the future by ASDF instead of in-order-to?
   ;; POIU is a parallel (multi-process build) extension of ASDF.  See
   ;; http://www.cliki.net/poiu
   (load-dependencies :accessor component-load-dependencies :initform nil)
   ;; In the ASDF object model, dependencies exist between *actions*
   ;; (an action is a pair of operation and component). They are represented
   ;; alists of operations to dependencies (other actions) in each component.
   ;; There are two kinds of dependencies, each stored in its own slot:
   ;; in-order-to and do-first dependencies. These two kinds are related to
   ;; the fact that some actions modify the filesystem,
   ;; whereas other actions modify the current image, and
   ;; this implies a difference in how to interpret timestamps.
   ;; in-order-to dependencies will trigger re-performing the action
   ;; when the timestamp of some dependency
   ;; makes the timestamp of current action out-of-date;
   ;; do-first dependencies do not trigger such re-performing.
   ;; Therefore, a FASL must be recompiled if it is obsoleted
   ;; by any of its FASL dependencies (in-order-to); but
   ;; it needn't be recompiled just because one of these dependencies
   ;; hasn't yet been loaded in the current image (do-first).
   ;; The names are crap, but they have been the official API since Dan Barlow's ASDF 1.52!
   (in-order-to :initform nil :initarg :in-order-to
                :accessor component-in-order-to)
   (do-first :initform nil :initarg :do-first
             :accessor component-do-first)
   ;; methods defined using the "inline" style inside a defsystem form:
   ;; need to store them somewhere so we can delete them when the system
   ;; is re-evaluated
   (inline-methods :accessor component-inline-methods :initform nil)
   (parent :initarg :parent :initform nil :reader component-parent)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (relative-pathname :initarg :pathname)
   (absolute-pathname)
   (operation-times :initform (make-hash-table)
                    :accessor component-operation-times)
   ;; XXX we should provide some atomic interface for updating the
   ;; component properties
   (properties :accessor component-properties :initarg :properties
               :initform nil)))

(defun* component-find-path (component)
  (reverse
   (loop :for c = component :then (component-parent c)
     :while c :collect (component-name c))))

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity nil)
    (format stream "~@<~{~S~^ ~}~@:>" (component-find-path c))))


;;;; methods: conditions

(defmethod print-object ((c missing-dependency) s)
  (format s "~@<~A, required by ~A~@:>"
          (call-next-method c nil) (missing-required-by c)))

(defun* sysdef-error (format &rest arguments)
  (error 'formatted-system-definition-error :format-control
         format :format-arguments arguments))

;;;; methods: components

(defmethod print-object ((c missing-component) s)
  (format s "~@<component ~S not found~@[ in ~A~]~@:>"
          (missing-requires c)
          (when (missing-parent c)
            (component-name (missing-parent c)))))

(defmethod print-object ((c missing-component-of-version) s)
  (format s "~@<component ~S does not match version ~A~@[ in ~A~]~@:>"
          (missing-requires c)
          (missing-version c)
          (when (missing-parent c)
            (component-name (missing-parent c)))))

(defmethod component-system ((component component))
  (aif (component-parent component)
       (component-system it)
       component))

(defvar *default-component-class* 'cl-source-file)

(defun* compute-module-components-by-name (module)
  (let ((hash (make-hash-table :test 'equal)))
    (setf (module-components-by-name module) hash)
    (loop :for c :in (module-components module)
      :for name = (component-name c)
      :for previous = (gethash name (module-components-by-name module))
      :do
      (when previous
        (error 'duplicate-names :name name))
      :do (setf (gethash name (module-components-by-name module)) c))
    hash))

(defclass module (component)
  ((components
    :initform nil
    :initarg :components
    :accessor module-components)
   (components-by-name
    :accessor module-components-by-name)
   ;; What to do if we can't satisfy a dependency of one of this module's
   ;; components.  This allows a limited form of conditional processing.
   (if-component-dep-fails
    :initform :fail
    :initarg :if-component-dep-fails
    :accessor module-if-component-dep-fails)
   (default-component-class
    :initform *default-component-class*
    :initarg :default-component-class
    :accessor module-default-component-class)))

(defun* component-parent-pathname (component)
  ;; No default anymore (in particular, no *default-pathname-defaults*).
  ;; If you force component to have a NULL pathname, you better arrange
  ;; for any of its children to explicitly provide a proper absolute pathname
  ;; wherever a pathname is actually wanted.
  (let ((parent (component-parent component)))
    (when parent
      (component-pathname parent))))

(defmethod component-pathname ((component component))
  (if (slot-boundp component 'absolute-pathname)
      (slot-value component 'absolute-pathname)
      (let ((pathname
             (merge-pathnames*
             (component-relative-pathname component)
             (pathname-directory-pathname (component-parent-pathname component)))))
        (unless (or (null pathname) (absolute-pathname-p pathname))
          (error "Invalid relative pathname ~S for component ~S"
                 pathname (component-find-path component)))
        (setf (slot-value component 'absolute-pathname) pathname)
        pathname)))

(defmethod component-property ((c component) property)
  (cdr (assoc property (slot-value c 'properties) :test #'equal)))

(defmethod (setf component-property) (new-value (c component) property)
  (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
    (if a
        (setf (cdr a) new-value)
        (setf (slot-value c 'properties)
              (acons property new-value (slot-value c 'properties)))))
  new-value)

(defclass system (module)
  ((description :accessor system-description :initarg :description)
   (long-description
    :accessor system-long-description :initarg :long-description)
   (author :accessor system-author :initarg :author)
   (maintainer :accessor system-maintainer :initarg :maintainer)
   (licence :accessor system-licence :initarg :licence
            :accessor system-license :initarg :license)
   (source-file :reader system-source-file :initarg :source-file
                :writer %set-system-source-file)
   (defsystem-depends-on :reader system-defsystem-depends-on :initarg :defsystem-depends-on)))

;;;; -------------------------------------------------------------------------
;;;; version-satisfies

(defmethod version-satisfies ((c component) version)
  (unless (and version (slot-boundp c 'version))
    (return-from version-satisfies t))
  (version-satisfies (component-version c) version))

(defmethod version-satisfies ((cver string) version)
  (let ((x (mapcar #'parse-integer
                   (split-string cver :separator ".")))
        (y (mapcar #'parse-integer
                   (split-string version :separator "."))))
    (labels ((bigger (x y)
               (cond ((not y) t)
                     ((not x) nil)
                     ((> (car x) (car y)) t)
                     ((= (car x) (car y))
                      (bigger (cdr x) (cdr y))))))
      (and (= (car x) (car y))
           (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))

;;;; -------------------------------------------------------------------------
;;;; Finding systems

(defun* make-defined-systems-table ()
  (make-hash-table :test 'equal))

(defvar *defined-systems* (make-defined-systems-table)
  "This is a hash table whose keys are strings, being the
names of the systems, and whose values are pairs, the first
element of which is a universal-time indicating when the
system definition was last updated, and the second element
of which is a system object.")

(defun* coerce-name (name)
  (typecase name
    (component (component-name name))
    (symbol (string-downcase (symbol-name name)))
    (string name)
    (t (sysdef-error "~@<invalid component designator ~A~@:>" name))))

(defun* system-registered-p (name)
  (gethash (coerce-name name) *defined-systems*))

(defun* clear-system (name)
  "Clear the entry for a system in the database of systems previously loaded.
Note that this does NOT in any way cause the code of the system to be unloaded."
  ;; There is no "unload" operation in Common Lisp, and a general such operation
  ;; cannot be portably written, considering how much CL relies on side-effects
  ;; of global data structures.
  ;; Note that this does a setf gethash instead of a remhash
  ;; this way there remains a hint in the *defined-systems* table
  ;; that the system was loaded at some point.
  (setf (gethash (coerce-name name) *defined-systems*) nil))

(defun* map-systems (fn)
  "Apply FN to each defined system.

FN should be a function of one argument. It will be
called with an object of type asdf:system."
  (maphash (lambda (_ datum)
             (declare (ignore _))
             (destructuring-bind (_ . def) datum
               (declare (ignore _))
               (funcall fn def)))
           *defined-systems*))

;;; for the sake of keeping things reasonably neat, we adopt a
;;; convention that functions in this list are prefixed SYSDEF-

(defparameter *system-definition-search-functions*
  '(sysdef-central-registry-search sysdef-source-registry-search sysdef-find-asdf))

(defun* system-definition-pathname (system)
  (let ((system-name (coerce-name system)))
    (or
     (some (lambda (x) (funcall x system-name))
           *system-definition-search-functions*)
     (let ((system-pair (system-registered-p system-name)))
       (and system-pair
            (system-source-file (cdr system-pair)))))))

(defvar *central-registry* nil
"A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or an expression
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))

This is for backward compatibilily.
Going forward, we recommend new users should be using the source-registry.
")

(defun* probe-asd (name defaults)
  (block nil
    (when (directory-pathname-p defaults)
      (let ((file
             (make-pathname
              :defaults defaults :version :newest :case :local
              :name name
              :type "asd")))
        (when (probe-file file)
          (return file)))
      #+(and (or win32 windows mswindows mingw32) (not cygwin) (not clisp))
      (let ((shortcut
             (make-pathname
              :defaults defaults :version :newest :case :local
              :name (concatenate 'string name ".asd")
              :type "lnk")))
        (when (probe-file shortcut)
          (let ((target (parse-windows-shortcut shortcut)))
            (when target
              (return (pathname target)))))))))

(defun* sysdef-central-registry-search (system)
  (let ((name (coerce-name system))
        (to-remove nil)
        (to-replace nil))
    (block nil
      (unwind-protect
           (dolist (dir *central-registry*)
             (let ((defaults (eval dir)))
               (when defaults
                 (cond ((directory-pathname-p defaults)
                        (let ((file (probe-asd name defaults)))
                          (when file
                            (return file))))
                       (t
                        (restart-case
                            (let* ((*print-circle* nil)
                                   (message
                                    (format nil
                                            "~@<While searching for system ~S: ~S evaluated to ~S which is not a directory.~@:>"
                                            system dir defaults)))
                              (error message))
                          (remove-entry-from-registry ()
                            :report "Remove entry from *central-registry* and continue"
                            (push dir to-remove))
                          (coerce-entry-to-directory ()
                            :report (lambda (s)
                                      (format s "Coerce entry to ~a, replace ~a and continue."
                                              (ensure-directory-pathname defaults) dir))
                            (push (cons dir (ensure-directory-pathname defaults)) to-replace))))))))
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

(defun* make-temporary-package ()
  (flet ((try (counter)
           (ignore-errors
             (make-package (format nil "~A~D" :asdf counter)
                           :use '(:cl :asdf)))))
    (do* ((counter 0 (+ counter 1))
          (package (try counter) (try counter)))
         (package package))))

(defun* safe-file-write-date (pathname)
  ;; If FILE-WRITE-DATE returns NIL, it's possible that
  ;; the user or some other agent has deleted an input file.
  ;; Also, generated files will not exist at the time planning is done
  ;; and calls operation-done-p which calls safe-file-write-date.
  ;; So it is very possible that we can't get a valid file-write-date,
  ;; and we can survive and we will continue the planning
  ;; as if the file were very old.
  ;; (or should we treat the case in a different, special way?)
  (or (and pathname (probe-file pathname) (file-write-date pathname))
      (progn
        (when (and pathname *asdf-verbose*)
          (warn "Missing FILE-WRITE-DATE for ~S: treating it as zero."
                pathname))
        0)))

(defmethod find-system (name &optional (error-p t))
  (find-system (coerce-name name) error-p))

(defmethod find-system ((name string) &optional (error-p t))
  (catch 'find-system
    (let* ((in-memory (system-registered-p name))
           (on-disk (system-definition-pathname name)))
      (when (and on-disk
                 (or (not in-memory)
                     (< (car in-memory) (safe-file-write-date on-disk))))
        (let ((package (make-temporary-package)))
          (unwind-protect
               (handler-bind
                   ((error (lambda (condition)
                             (error 'load-system-definition-error
                                    :name name :pathname on-disk
                                    :condition condition))))
                 (let ((*package* package))
                   (asdf-message
                    "~&~@<; ~@;loading system definition from ~A into ~A~@:>~%"
                    on-disk *package*)
                   (load on-disk)))
            (delete-package package))))
      (let ((in-memory (system-registered-p name)))
        (cond
          (in-memory
           (when on-disk
             (setf (car in-memory) (safe-file-write-date on-disk)))
           (cdr in-memory))
          (error-p
           (error 'missing-component :requires name)))))))

(defun* register-system (name system)
  (asdf-message "~&~@<; ~@;registering ~A as ~A~@:>~%" system name)
  (setf (gethash (coerce-name name) *defined-systems*)
        (cons (get-universal-time) system)))

(defun* find-system-fallback (requested fallback &rest keys &key source-file &allow-other-keys)
  (setf fallback (coerce-name fallback)
        source-file (or source-file *compile-file-truename* *load-truename*)
        requested (coerce-name requested))
  (when (equal requested fallback)
    (let* ((registered (cdr (gethash fallback *defined-systems*)))
           (system (or registered
                       (apply 'make-instance 'system
			      :name fallback :source-file source-file keys))))
      (unless registered
        (register-system fallback system))
      (throw 'find-system system))))

(defun* sysdef-find-asdf (name)
  (find-system-fallback name "asdf")) ;; :version *asdf-version* wouldn't be updated when ASDF is updated.


;;;; -------------------------------------------------------------------------
;;;; Finding components

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

(defmethod find-component ((module module) (name string))
  (unless (slot-boundp module 'components-by-name) ;; SBCL may miss the u-i-f-r-c method!!!
    (compute-module-components-by-name module))
  (values (gethash name (module-components-by-name module))))

(defmethod find-component ((component component) (name symbol))
  (if name
      (find-component component (coerce-name name))
      component))

(defmethod find-component ((module module) (name cons))
  (find-component (find-component module (car name)) (cdr name)))


;;; component subclasses

(defclass source-file (component)
  ((type :accessor source-file-explicit-type :initarg :type :initform nil)))

(defclass cl-source-file (source-file)
  ((type :initform "lisp")))
(defclass c-source-file (source-file)
  ((type :initform "c")))
(defclass java-source-file (source-file)
  ((type :initform "java")))
(defclass static-file (source-file) ())
(defclass doc-file (static-file) ())
(defclass html-file (doc-file)
  ((type :initform "html")))

(defmethod source-file-type ((component module) (s module))
  (declare (ignorable component s))
  :directory)
(defmethod source-file-type ((component source-file) (s module))
  (declare (ignorable s))
  (source-file-explicit-type component))

(defun* merge-component-name-type (name &key type defaults)
  ;; The defaults are required notably because they provide the default host
  ;; to the below make-pathname, which may crucially matter to people using
  ;; merge-pathnames with non-default hosts,  e.g. for logical-pathnames.
  ;; NOTE that the host and device slots will be taken from the defaults,
  ;; but that should only matter if you either (a) use absolute pathnames, or
  ;; (b) later merge relative pathnames with CL:MERGE-PATHNAMES instead of
  ;; ASDF:MERGE-PATHNAMES*
  (etypecase name
    (pathname
     name)
    (symbol
     (merge-component-name-type (string-downcase name) :type type :defaults defaults))
    (string
     (multiple-value-bind (relative path filename)
         (component-name-to-pathname-components name :force-directory (eq type :directory)
                                                :force-relative t)
       (multiple-value-bind (name type)
           (cond
             ((or (eq type :directory) (null filename))
              (values nil nil))
             (type
              (values filename type))
             (t
              (split-name-type filename)))
         (let* ((defaults (pathname (or defaults *default-pathname-defaults*)))
                (host (pathname-host defaults))
                (device (pathname-device defaults)))
           (make-pathname :directory `(,relative ,@path)
                          :name name :type type
                          :host host :device device)))))))

(defmethod component-relative-pathname ((component component))
  (merge-component-name-type
   (or (slot-value component 'relative-pathname)
       (component-name component))
   :type (source-file-type component (component-system component))
   :defaults (component-parent-pathname component)))

;;;; -------------------------------------------------------------------------
;;;; Operations

;;; one of these is instantiated whenever #'operate is called

(defclass operation ()
  (
   ;; as of danb's 2003-03-16 commit e0d02781, :force can be:
   ;; T to force the inside of existing system,
   ;;   but not recurse to other systems we depend on.
   ;; :ALL (or any other atom) to force all systems
   ;;   including other systems we depend on.
   ;; (SYSTEM1 SYSTEM2 ... SYSTEMN)
   ;;   to force systems named in a given list
   ;; However, but this feature never worked before ASDF 1.700 and is currently cerror'ed out.
   (forced :initform nil :initarg :force :accessor operation-forced)
   (original-initargs :initform nil :initarg :original-initargs
                      :accessor operation-original-initargs)
   (visited-nodes :initform (make-hash-table :test 'equal) :accessor operation-visited-nodes)
   (visiting-nodes :initform (make-hash-table :test 'equal) :accessor operation-visiting-nodes)
   (parent :initform nil :initarg :parent :accessor operation-parent)))

(defmethod print-object ((o operation) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (ignore-errors
      (prin1 (operation-original-initargs o) stream))))

(defmethod shared-initialize :after ((operation operation) slot-names
                                     &key force
                                     &allow-other-keys)
  (declare (ignorable operation slot-names force))
  ;; empty method to disable initarg validity checking
  (values))

(defun* node-for (o c)
  (cons (class-name (class-of o)) c))

(defmethod operation-ancestor ((operation operation))
  (aif (operation-parent operation)
       (operation-ancestor it)
       operation))


(defun* make-sub-operation (c o dep-c dep-o)
  "C is a component, O is an operation, DEP-C is another
component, and DEP-O, confusingly enough, is an operation
class specifier, not an operation."
  (let* ((args (copy-list (operation-original-initargs o)))
         (force-p (getf args :force)))
    ;; note explicit comparison with T: any other non-NIL force value
    ;; (e.g. :recursive) will pass through
    (cond ((and (null (component-parent c))
                (null (component-parent dep-c))
                (not (eql c dep-c)))
           (when (eql force-p t)
             (setf (getf args :force) nil))
           (apply #'make-instance dep-o
                  :parent o
                  :original-initargs args args))
          ((subtypep (type-of o) dep-o)
           o)
          (t
           (apply #'make-instance dep-o
                  :parent o :original-initargs args args)))))


(defmethod visit-component ((o operation) (c component) data)
  (unless (component-visited-p o c)
    (setf (gethash (node-for o c)
                   (operation-visited-nodes (operation-ancestor o)))
          (cons t data))))

(defmethod component-visited-p ((o operation) (c component))
  (gethash (node-for o c)
           (operation-visited-nodes (operation-ancestor o))))

(defmethod (setf visiting-component) (new-value operation component)
  ;; MCL complains about unused lexical variables
  (declare (ignorable operation component))
  new-value)

(defmethod (setf visiting-component) (new-value (o operation) (c component))
  (let ((node (node-for o c))
        (a (operation-ancestor o)))
    (if new-value
        (setf (gethash node (operation-visiting-nodes a)) t)
        (remhash node (operation-visiting-nodes a)))
    new-value))

(defmethod component-visiting-p ((o operation) (c component))
  (let ((node (node-for o c)))
    (gethash node (operation-visiting-nodes (operation-ancestor o)))))

(defmethod component-depends-on ((op-spec symbol) (c component))
  (component-depends-on (make-instance op-spec) c))

(defmethod component-depends-on ((o operation) (c component))
  (cdr (assoc (class-name (class-of o))
              (component-in-order-to c))))

(defmethod component-self-dependencies ((o operation) (c component))
  (let ((all-deps (component-depends-on o c)))
    (remove-if-not (lambda (x)
                     (member (component-name c) (cdr x) :test #'string=))
                   all-deps)))

(defmethod input-files ((operation operation) (c component))
  (let ((parent (component-parent c))
        (self-deps (component-self-dependencies operation c)))
    (if self-deps
        (mapcan (lambda (dep)
                  (destructuring-bind (op name) dep
                    (output-files (make-instance op)
                                  (find-component parent name))))
                self-deps)
        ;; no previous operations needed?  I guess we work with the
        ;; original source file, then
        (list (component-pathname c)))))

(defmethod input-files ((operation operation) (c module))
  (declare (ignorable operation c))
  nil)

(defmethod component-operation-time (o c)
  (gethash (type-of o) (component-operation-times c)))

(defmethod operation-done-p ((o operation) (c component))
  (let ((out-files (output-files o c))
        (in-files (input-files o c))
        (op-time (component-operation-time o c)))
    (flet ((earliest-out ()
             (reduce #'min (mapcar #'safe-file-write-date out-files)))
           (latest-in ()
             (reduce #'max (mapcar #'safe-file-write-date in-files))))
      (cond
        ((and (not in-files) (not out-files))
         ;; arbitrary decision: an operation that uses nothing to
         ;; produce nothing probably isn't doing much.
         ;; e.g. operations on systems, modules that have no immediate action,
         ;; but are only meaningful through traversed dependencies
         t)
        ((not out-files)
         ;; an operation without output-files is probably meant
         ;; for its side-effects in the current image,
         ;; assumed to be idem-potent,
         ;; e.g. LOAD-OP or LOAD-SOURCE-OP of some CL-SOURCE-FILE.
         (and op-time (>= op-time (latest-in))))
        ((not in-files)
         ;; an operation without output-files and no input-files
         ;; is probably meant for its side-effects on the file-system,
         ;; assumed to have to be done everytime.
         ;; (I don't think there is any such case in ASDF unless extended)
         nil)
        (t
         ;; an operation with both input and output files is assumed
         ;; as computing the latter from the former,
         ;; assumed to have been done if the latter are all older
         ;; than the former.
         ;; e.g. COMPILE-OP of some CL-SOURCE-FILE.
         ;; We use >= instead of > to play nice with generated files.
         ;; This opens a race condition if an input file is changed
         ;; after the output is created but within the same second
         ;; of filesystem time; but the same race condition exists
         ;; whenever the computation from input to output takes more
         ;; than one second of filesystem time (or just crosses the
         ;; second). So that's cool.
         (and
          (every #'probe-file in-files)
          (every #'probe-file out-files)
          (>= (earliest-out) (latest-in))))))))



;;; For 1.700 I've done my best to refactor TRAVERSE
;;; by splitting it up in a bunch of functions,
;;; so as to improve the collection and use-detection algorithm. --fare
;;; The protocol is as follows: we pass around operation, dependency,
;;; bunch of other stuff, and a force argument. Return a force flag.
;;; The returned flag is T if anything has changed that requires a rebuild.
;;; The force argument is a list of components that will require a rebuild
;;; if the flag is T, at which point whoever returns the flag has to
;;; mark them all as forced, and whoever recurses again can use a NIL list
;;; as a further argument.

(defvar *forcing* nil
  "This dynamically-bound variable is used to force operations in
recursive calls to traverse.")

(defgeneric* do-traverse (operation component collect))

(defun* %do-one-dep (operation c collect required-op required-c required-v)
  ;; collects a partial plan that results from performing required-op
  ;; on required-c, possibly with a required-vERSION
  (let* ((dep-c (or (let ((d (find-component (component-parent c) required-c)))
                      (and d (version-satisfies d required-v) d))
                    (if required-v
                        (error 'missing-dependency-of-version
                               :required-by c
                               :version required-v
                               :requires required-c)
                        (error 'missing-dependency
                               :required-by c
                               :requires required-c))))
         (op (make-sub-operation c operation dep-c required-op)))
    (do-traverse op dep-c collect)))

(defun* do-one-dep (operation c collect required-op required-c required-v)
  ;; this function is a thin, error-handling wrapper around %do-one-dep.
  ;; Collects a partial plan per that function.
  (loop
    (restart-case
        (return (%do-one-dep operation c collect
                             required-op required-c required-v))
      (retry ()
        :report (lambda (s)
                  (format s "~@<Retry loading component ~S.~@:>"
                          (component-find-path required-c)))
        :test
        (lambda (c)
          (or (null c)
              (and (typep c 'missing-dependency)
                   (equalp (missing-requires c)
                           required-c))))))))

(defun* do-dep (operation c collect op dep)
  ;; type of arguments uncertain:
  ;; op seems to at least potentially be a symbol, rather than an operation
  ;; dep is a list of component names
  (cond ((eq op 'feature)
         (if (member (car dep) *features*)
             nil
             (error 'missing-dependency
                    :required-by c
                    :requires (car dep))))
        (t
         (let ((flag nil))
           (flet ((dep (op comp ver)
                    (when (do-one-dep operation c collect
                                      op comp ver)
                      (setf flag t))))
             (dolist (d dep)
               (if (atom d)
                   (dep op d nil)
                   ;; structured dependencies --- this parses keywords
                   ;; the keywords could be broken out and cleanly (extensibly)
                   ;; processed by EQL methods
                   (cond ((eq :version (first d))
                          ;; https://bugs.launchpad.net/asdf/+bug/527788
                          (dep op (second d) (third d)))
                         ;; This particular subform is not documented and
                         ;; has always been broken in the past.
                         ;; Therefore no one uses it, and I'm cerroring it out,
                         ;; after fixing it
                         ;; See https://bugs.launchpad.net/asdf/+bug/518467
                         ((eq :feature (first d))
                          (cerror "Continue nonetheless."
                                  "Congratulations, you're the first ever user of FEATURE dependencies! Please contact the asdf-devel mailing-list.")
                          (when (find (second d) *features* :test 'string-equal)
                            (dep op (third d) nil)))
                         (t
                          (error "Bad dependency ~a.  Dependencies must be (:version <version>), (:feature <feature> [version]), or a name" d))))))
           flag))))

(defvar *visit-count* 0) ; counter that allows to sort nodes from operation-visited-nodes

(defun* do-collect (collect x)
  (funcall collect x))

(defmethod do-traverse ((operation operation) (c component) collect)
  (let ((flag nil)) ;; return value: must we rebuild this and its dependencies?
    (labels
        ((update-flag (x)
           (when x
             (setf flag t)))
         (dep (op comp)
           (update-flag (do-dep operation c collect op comp))))
      ;; Have we been visited yet? If so, just process the result.
      (aif (component-visited-p operation c)
           (progn
             (update-flag (cdr it))
             (return-from do-traverse flag)))
      ;; dependencies
      (when (component-visiting-p operation c)
        (error 'circular-dependency :components (list c)))
      (setf (visiting-component operation c) t)
      (unwind-protect
           (progn
             ;; first we check and do all the dependencies for the module.
             ;; Operations planned in this loop will show up
             ;; in the results, and are consumed below.
             (let ((*forcing* nil))
               ;; upstream dependencies are never forced to happen just because
               ;; the things that depend on them are....
               (loop
                 :for (required-op . deps) :in (component-depends-on operation c)
                 :do (dep required-op deps)))
             ;; constituent bits
             (let ((module-ops
                    (when (typep c 'module)
                      (let ((at-least-one nil)
                            ;; This is set based on the results of the
                            ;; dependencies and whether we are in the
                            ;; context of a *forcing* call...
                            ;; inter-system dependencies do NOT trigger
                            ;; building components
                            (*forcing*
                             (or *forcing*
                                 (and flag (not (typep c 'system)))))
                            (error nil))
                        (while-collecting (internal-collect)
                          (dolist (kid (module-components c))
                            (handler-case
                                (update-flag
                                 (do-traverse operation kid #'internal-collect))
                              (missing-dependency (condition)
                                (when (eq (module-if-component-dep-fails c)
                                          :fail)
                                  (error condition))
                                (setf error condition))
                              (:no-error (c)
                                (declare (ignore c))
                                (setf at-least-one t))))
                          (when (and (eq (module-if-component-dep-fails c)
                                         :try-next)
                                     (not at-least-one))
                            (error error)))))))
               (update-flag
                (or
                 *forcing*
                 (not (operation-done-p operation c))
                 ;; For sub-operations, check whether
                 ;; the original ancestor operation was forced,
                 ;; or names us amongst an explicit list of things to force...
                 ;; except that this check doesn't distinguish
                 ;; between all the things with a given name. Sigh.
                 ;; BROKEN!
                 (let ((f (operation-forced
                           (operation-ancestor operation))))
                   (and f (or (not (consp f)) ;; T or :ALL
                              (and (typep c 'system) ;; list of names of systems to force
                                   (member (component-name c) f
                                           :test #'string=)))))))
               (when flag
                 (let ((do-first (cdr (assoc (class-name (class-of operation))
                                             (component-do-first c)))))
                   (loop :for (required-op . deps) :in do-first
                     :do (do-dep operation c collect required-op deps)))
                 (do-collect collect (vector module-ops))
                 (do-collect collect (cons operation c)))))
             (setf (visiting-component operation c) nil)))
      (visit-component operation c (when flag (incf *visit-count*)))
      flag))

(defun* flatten-tree (l)
  ;; You collected things into a list.
  ;; Most elements are just things to collect again.
  ;; A (simple-vector 1) indicate that you should recurse into its contents.
  ;; This way, in two passes (rather than N being the depth of the tree),
  ;; you can collect things with marginally constant-time append,
  ;; achieving linear time collection instead of quadratic time.
  (while-collecting (c)
    (labels ((r (x)
               (if (typep x '(simple-vector 1))
                   (r* (svref x 0))
                   (c x)))
             (r* (l)
               (dolist (x l) (r x))))
      (r* l))))

(defmethod traverse ((operation operation) (c component))
  ;; cerror'ing a feature that seems to have NEVER EVER worked
  ;; ever since danb created it in his 2003-03-16 commit e0d02781.
  ;; It was both fixed and disabled in the 1.700 rewrite.
  (when (consp (operation-forced operation))
    (cerror "Continue nonetheless."
            "Congratulations, you're the first ever user of the :force (list of system names) feature! Please contact the asdf-devel mailing-list to collect a cookie.")
    (setf (operation-forced operation)
          (mapcar #'coerce-name (operation-forced operation))))
  (flatten-tree
   (while-collecting (collect)
     (let ((*visit-count* 0))
       (do-traverse operation c #'collect)))))

(defmethod perform ((operation operation) (c source-file))
  (sysdef-error
   "~@<required method PERFORM not implemented for operation ~A, component ~A~@:>"
   (class-of operation) (class-of c)))

(defmethod perform ((operation operation) (c module))
  (declare (ignorable operation c))
  nil)

(defmethod explain ((operation operation) (component component))
  (asdf-message "~&;;; ~A~%" (operation-description operation component)))

(defmethod operation-description (operation component)
  (format nil "~A on component ~S" (class-of operation) (component-find-path component)))

;;;; -------------------------------------------------------------------------
;;;; compile-op

(defclass compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (on-warnings :initarg :on-warnings :accessor operation-on-warnings
                :initform *compile-file-warnings-behaviour*)
   (on-failure :initarg :on-failure :accessor operation-on-failure
               :initform *compile-file-failure-behaviour*)
   (flags :initarg :flags :accessor compile-op-flags
          :initform #-ecl nil #+ecl '(:system-p t))))

(defun output-file (operation component)
  "The unique output file of performing OPERATION on COMPONENT"
  (let ((files (output-files operation component)))
    (assert (length=n-p files 1))
    (first files)))

(defmethod perform :before ((operation compile-op) (c source-file))
  (map nil #'ensure-directories-exist (output-files operation c)))

#+ecl
(defmethod perform :after ((o compile-op) (c cl-source-file))
  ;; Note how we use OUTPUT-FILES to find the binary locations
  ;; This allows the user to override the names.
  (let* ((files (output-files o c))
         (object (first files))
         (fasl (second files)))
    (c:build-fasl fasl :lisp-files (list object))))

(defmethod perform :after ((operation operation) (c component))
  (setf (gethash (type-of operation) (component-operation-times c))
        (get-universal-time)))

(declaim (ftype (function ((or pathname string)
                           &rest t &key (:output-file t) &allow-other-keys)
                          (values t t t))
                compile-file*))

;;; perform is required to check output-files to find out where to put
;;; its answers, in case it has been overridden for site policy
(defmethod perform ((operation compile-op) (c cl-source-file))
  #-:broken-fasl-loader
  (let ((source-file (component-pathname c))
        ;; on some implementations, there are more than one output-file,
        ;; but the first one should always be the primary fasl that gets loaded.
        (output-file (first (output-files operation c)))
        (*compile-file-warnings-behaviour* (operation-on-warnings operation))
        (*compile-file-failure-behaviour* (operation-on-failure operation)))
    (multiple-value-bind (output warnings-p failure-p)
        (apply #'compile-file* source-file :output-file output-file
               (compile-op-flags operation))
      (when warnings-p
        (case (operation-on-warnings operation)
          (:warn (warn
                  "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>"
                  operation c))
          (:error (error 'compile-warned :component c :operation operation))
          (:ignore nil)))
      (when failure-p
        (case (operation-on-failure operation)
          (:warn (warn
                  "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>"
                  operation c))
          (:error (error 'compile-failed :component c :operation operation))
          (:ignore nil)))
      (unless output
        (error 'compile-error :component c :operation operation)))))

(defmethod output-files ((operation compile-op) (c cl-source-file))
  (declare (ignorable operation))
  (let ((p (lispize-pathname (component-pathname c))))
    #-:broken-fasl-loader
    (list (compile-file-pathname p #+ecl :type #+ecl :object)
          #+ecl (compile-file-pathname p :type :fasl))
    #+:broken-fasl-loader (list p)))

(defmethod perform ((operation compile-op) (c static-file))
  (declare (ignorable operation c))
  nil)

(defmethod output-files ((operation compile-op) (c static-file))
  (declare (ignorable operation c))
  nil)

(defmethod input-files ((operation compile-op) (c static-file))
  (declare (ignorable operation c))
  nil)

(defmethod operation-description ((operation compile-op) component)
  (declare (ignorable operation))
  (format nil "compiling component ~S" (component-find-path component)))

;;;; -------------------------------------------------------------------------
;;;; load-op

(defclass basic-load-op (operation) ())

(defclass load-op (basic-load-op) ())

(defmethod perform ((o load-op) (c cl-source-file))
  (map () #'load
       #-ecl (input-files o c)
       #+ecl (loop :for i :in (input-files o c)
               :unless (string= (pathname-type i) "fas")
               :collect (compile-file-pathname (lispize-pathname i)))))

(defmethod perform-with-restarts (operation component)
  (perform operation component))

(defmethod perform-with-restarts ((o load-op) (c cl-source-file))
  (declare (ignorable o))
  (loop :with state = :initial
    :until (or (eq state :success)
               (eq state :failure)) :do
    (case state
      (:recompiled
       (setf state :failure)
       (call-next-method)
       (setf state :success))
      (:failed-load
       (setf state :recompiled)
       (perform (make-instance 'compile-op) c))
      (t
       (with-simple-restart
           (try-recompiling "Recompile ~a and try loading it again"
                            (component-name c))
         (setf state :failed-load)
         (call-next-method)
         (setf state :success))))))

(defmethod perform-with-restarts ((o compile-op) (c cl-source-file))
  (loop :with state = :initial
    :until (or (eq state :success)
               (eq state :failure)) :do
    (case state
      (:recompiled
       (setf state :failure)
       (call-next-method)
       (setf state :success))
      (:failed-compile
       (setf state :recompiled)
       (perform-with-restarts o c))
      (t
       (with-simple-restart
           (try-recompiling "Try recompiling ~a"
                            (component-name c))
         (setf state :failed-compile)
         (call-next-method)
         (setf state :success))))))

(defmethod perform ((operation load-op) (c static-file))
  (declare (ignorable operation c))
  nil)

(defmethod operation-done-p ((operation load-op) (c static-file))
  (declare (ignorable operation c))
  t)

(defmethod output-files ((operation operation) (c component))
  (declare (ignorable operation c))
  nil)

(defmethod component-depends-on ((operation load-op) (c component))
  (declare (ignorable operation))
  (cons (list 'compile-op (component-name c))
        (call-next-method)))

(defmethod operation-description ((operation load-op) component)
  (declare (ignorable operation))
  (format nil "loading component ~S" (component-find-path component)))


;;;; -------------------------------------------------------------------------
;;;; load-source-op

(defclass load-source-op (basic-load-op) ())

(defmethod perform ((o load-source-op) (c cl-source-file))
  (declare (ignorable o))
  (let ((source (component-pathname c)))
    (setf (component-property c 'last-loaded-as-source)
          (and (load source)
               (get-universal-time)))))

(defmethod perform ((operation load-source-op) (c static-file))
  (declare (ignorable operation c))
  nil)

(defmethod output-files ((operation load-source-op) (c component))
  (declare (ignorable operation c))
  nil)

;;; FIXME: we simply copy load-op's dependencies.  this is Just Not Right.
(defmethod component-depends-on ((o load-source-op) (c component))
  (declare (ignorable o))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (component-in-order-to c)))))
    (mapcar (lambda (dep)
              (if (eq (car dep) 'load-op)
                  (cons 'load-source-op (cdr dep))
                  dep))
            what-would-load-op-do)))

(defmethod operation-done-p ((o load-source-op) (c source-file))
  (declare (ignorable o))
  (if (or (not (component-property c 'last-loaded-as-source))
          (> (safe-file-write-date (component-pathname c))
             (component-property c 'last-loaded-as-source)))
      nil t))

(defmethod operation-description ((operation load-source-op) component)
  (declare (ignorable operation))
  (format nil "loading component ~S" (component-find-path component)))


;;;; -------------------------------------------------------------------------
;;;; test-op

(defclass test-op (operation) ())

(defmethod perform ((operation test-op) (c component))
  (declare (ignorable operation c))
  nil)

(defmethod operation-done-p ((operation test-op) (c system))
  "Testing a system is _never_ done."
  (declare (ignorable operation c))
  nil)

(defmethod component-depends-on :around ((o test-op) (c system))
  (declare (ignorable o))
  (cons `(load-op ,(component-name c)) (call-next-method)))


;;;; -------------------------------------------------------------------------
;;;; Invoking Operations

(defgeneric* operate (operation-class system &key &allow-other-keys))

(defmethod operate (operation-class system &rest args
                    &key ((:verbose *asdf-verbose*) *asdf-verbose*) version force
                    &allow-other-keys)
  (declare (ignore force))
  (let* ((*package* *package*)
         (*readtable* *readtable*)
         (op (apply #'make-instance operation-class
                    :original-initargs args
                    args))
         (*verbose-out* (if *asdf-verbose* *standard-output* (make-broadcast-stream)))
         (system (if (typep system 'component) system (find-system system))))
    (unless (version-satisfies system version)
      (error 'missing-component-of-version :requires system :version version))
    (let ((steps (traverse op system)))
      (with-compilation-unit ()
        (loop :for (op . component) :in steps :do
          (loop
            (restart-case
                (progn
                  (perform-with-restarts op component)
                  (return))
              (retry ()
                :report
                (lambda (s)
                  (format s "~@<Retry ~A.~@:>" (operation-description op component))))
              (accept ()
                :report
                (lambda (s)
                  (format s "~@<Continue, treating ~A as having been successful.~@:>"
                          (operation-description op component)))
                (setf (gethash (type-of op)
                               (component-operation-times component))
                      (get-universal-time))
                (return))))))
      (values op steps))))

(defun* oos (operation-class system &rest args &key force verbose version
            &allow-other-keys)
  (declare (ignore force verbose version))
  (apply #'operate operation-class system args))

(let ((operate-docstring
  "Operate does three things:

1. It creates an instance of OPERATION-CLASS using any keyword parameters
as initargs.
2. It finds the  asdf-system specified by SYSTEM (possibly loading
it from disk).
3. It then calls TRAVERSE with the operation and system as arguments

The traverse operation is wrapped in WITH-COMPILATION-UNIT and error
handling code. If a VERSION argument is supplied, then operate also
ensures that the system found satisfies it using the VERSION-SATISFIES
method.

Note that dependencies may cause the operation to invoke other
operations on the system or its components: the new operations will be
created with the same initargs as the original one.
"))
  (setf (documentation 'oos 'function)
        (format nil
                "Short for _operate on system_ and an alias for the OPERATE function. ~&~&~a"
                operate-docstring))
  (setf (documentation 'operate 'function)
        operate-docstring))

(defun* load-system (system &rest args &key force verbose version
                    &allow-other-keys)
  "Shorthand for `(operate 'asdf:load-op system)`. See OPERATE for
details."
  (declare (ignore force verbose version))
  (apply #'operate 'load-op system args)
  t)

(defun* compile-system (system &rest args &key force verbose version
                       &allow-other-keys)
  "Shorthand for `(operate 'asdf:compile-op system)`. See OPERATE
for details."
  (declare (ignore force verbose version))
  (apply #'operate 'compile-op system args)
  t)

(defun* test-system (system &rest args &key force verbose version
                    &allow-other-keys)
  "Shorthand for `(operate 'asdf:test-op system)`. See OPERATE for
details."
  (declare (ignore force verbose version))
  (apply #'operate 'test-op system args)
  t)

;;;; -------------------------------------------------------------------------
;;;; Defsystem

(defun* load-pathname ()
  (let ((pn (or *load-pathname* *compile-file-pathname*)))
    (if *resolve-symlinks*
        (and pn (resolve-symlinks pn))
        pn)))

(defun* determine-system-pathname (pathname pathname-supplied-p)
  ;; The defsystem macro calls us to determine
  ;; the pathname of a system as follows:
  ;; 1. the one supplied,
  ;; 2. derived from *load-pathname* via load-pathname
  ;; 3. taken from the *default-pathname-defaults* via default-directory
  (let* ((file-pathname (load-pathname))
         (directory-pathname (and file-pathname (pathname-directory-pathname file-pathname))))
    (or (and pathname-supplied-p (merge-pathnames* pathname directory-pathname))
        directory-pathname
        (default-directory))))

(defmacro defsystem (name &body options)
  (destructuring-bind (&key (pathname nil pathname-arg-p) (class 'system)
                            defsystem-depends-on &allow-other-keys)
      options
    (let ((component-options (remove-keys '(:class) options)))
      `(progn
         ;; system must be registered before we parse the body, otherwise
         ;; we recur when trying to find an existing system of the same name
         ;; to reuse options (e.g. pathname) from
         ,@(loop :for system :in defsystem-depends-on
             :collect `(load-system ,system))
         (let ((s (system-registered-p ',name)))
           (cond ((and s (eq (type-of (cdr s)) ',class))
                  (setf (car s) (get-universal-time)))
                 (s
                  (change-class (cdr s) ',class))
                 (t
                  (register-system (quote ,name)
                                   (make-instance ',class :name ',name))))
           (%set-system-source-file (load-pathname)
                                    (cdr (system-registered-p ',name))))
         (parse-component-form
          nil (list*
               :module (coerce-name ',name)
               :pathname
               ,(determine-system-pathname pathname pathname-arg-p)
               ',component-options))))))

(defun* class-for-type (parent type)
  (or (loop :for symbol :in (list
                             (unless (keywordp type) type)
                             (find-symbol (symbol-name type) *package*)
                             (find-symbol (symbol-name type) :asdf))
        :for class = (and symbol (find-class symbol nil))
        :when (and class (subtypep class 'component))
        :return class)
      (and (eq type :file)
           (or (module-default-component-class parent)
               (find-class *default-component-class*)))
      (sysdef-error "~@<don't recognize component type ~A~@:>" type)))

(defun* maybe-add-tree (tree op1 op2 c)
  "Add the node C at /OP1/OP2 in TREE, unless it's there already.
Returns the new tree (which probably shares structure with the old one)"
  (let ((first-op-tree (assoc op1 tree)))
    (if first-op-tree
        (progn
          (aif (assoc op2 (cdr first-op-tree))
               (if (find c (cdr it))
                   nil
                   (setf (cdr it) (cons c (cdr it))))
               (setf (cdr first-op-tree)
                     (acons op2 (list c) (cdr first-op-tree))))
          tree)
        (acons op1 (list (list op2 c)) tree))))

(defun* union-of-dependencies (&rest deps)
  (let ((new-tree nil))
    (dolist (dep deps)
      (dolist (op-tree dep)
        (dolist (op  (cdr op-tree))
          (dolist (c (cdr op))
            (setf new-tree
                  (maybe-add-tree new-tree (car op-tree) (car op) c))))))
    new-tree))


(defvar *serial-depends-on* nil)

(defun* sysdef-error-component (msg type name value)
  (sysdef-error (concatenate 'string msg
                             "~&The value specified for ~(~A~) ~A is ~S")
                type name value))

(defun* check-component-input (type name weakly-depends-on
                              depends-on components in-order-to)
  "A partial test of the values of a component."
  (unless (listp depends-on)
    (sysdef-error-component ":depends-on must be a list."
                            type name depends-on))
  (unless (listp weakly-depends-on)
    (sysdef-error-component ":weakly-depends-on must be a list."
                            type name weakly-depends-on))
  (unless (listp components)
    (sysdef-error-component ":components must be NIL or a list of components."
                            type name components))
  (unless (and (listp in-order-to) (listp (car in-order-to)))
    (sysdef-error-component ":in-order-to must be NIL or a list of components."
                            type name in-order-to)))

(defun* %remove-component-inline-methods (component)
  (dolist (name +asdf-methods+)
    (map ()
         ;; this is inefficient as most of the stored
         ;; methods will not be for this particular gf
         ;; But this is hardly performance-critical
         (lambda (m)
           (remove-method (symbol-function name) m))
         (component-inline-methods component)))
  ;; clear methods, then add the new ones
  (setf (component-inline-methods component) nil))

(defun* %define-component-inline-methods (ret rest)
  (dolist (name +asdf-methods+)
    (let ((keyword (intern (symbol-name name) :keyword)))
      (loop :for data = rest :then (cddr data)
        :for key = (first data)
        :for value = (second data)
        :while data
        :when (eq key keyword) :do
        (destructuring-bind (op qual (o c) &body body) value
          (pushnew
           (eval `(defmethod ,name ,qual ((,o ,op) (,c (eql ,ret)))
                             ,@body))
           (component-inline-methods ret)))))))

(defun* %refresh-component-inline-methods (component rest)
  (%remove-component-inline-methods component)
  (%define-component-inline-methods component rest))

(defun* parse-component-form (parent options)
  (destructuring-bind
        (type name &rest rest &key
              ;; the following list of keywords is reproduced below in the
              ;; remove-keys form.  important to keep them in sync
              components pathname default-component-class
              perform explain output-files operation-done-p
              weakly-depends-on
              depends-on serial in-order-to
              ;; list ends
              &allow-other-keys) options
    (declare (ignorable perform explain output-files operation-done-p))
    (check-component-input type name weakly-depends-on depends-on components in-order-to)

    (when (and parent
               (find-component parent name)
               ;; ignore the same object when rereading the defsystem
               (not
                (typep (find-component parent name)
                       (class-for-type parent type))))
      (error 'duplicate-names :name name))

    (let* ((other-args (remove-keys
                        '(components pathname default-component-class
                          perform explain output-files operation-done-p
                          weakly-depends-on
                          depends-on serial in-order-to)
                        rest))
           (ret
            (or (find-component parent name)
                (make-instance (class-for-type parent type)))))
      (when weakly-depends-on
        (appendf depends-on (remove-if (complement #'find-system) weakly-depends-on)))
      (when *serial-depends-on*
        (push *serial-depends-on* depends-on))
      (apply #'reinitialize-instance ret
             :name (coerce-name name)
             :pathname pathname
             :parent parent
             other-args)
      (component-pathname ret) ; eagerly compute the absolute pathname
      (when (typep ret 'module)
        (setf (module-default-component-class ret)
              (or default-component-class
                  (and (typep parent 'module)
                       (module-default-component-class parent))))
        (let ((*serial-depends-on* nil))
          (setf (module-components ret)
                (loop
                  :for c-form :in components
                  :for c = (parse-component-form ret c-form)
                  :for name = (component-name c)
                  :collect c
                  :when serial :do (setf *serial-depends-on* name))))
        (compute-module-components-by-name ret))

      (setf (component-load-dependencies ret) depends-on) ;; Used by POIU

      (setf (component-in-order-to ret)
            (union-of-dependencies
             in-order-to
             `((compile-op (compile-op ,@depends-on))
               (load-op (load-op ,@depends-on)))))
      (setf (component-do-first ret) `((compile-op (load-op ,@depends-on))))

      (%refresh-component-inline-methods ret rest)
      ret)))

;;;; ---------------------------------------------------------------------------
;;;; run-shell-command
;;;;
;;;; run-shell-command functions for other lisp implementations will be
;;;; gratefully accepted, if they do the same thing.
;;;; If the docstring is ambiguous, send a bug report.
;;;;
;;;; We probably should move this functionality to its own system and deprecate
;;;; use of it from the asdf package. However, this would break unspecified
;;;; existing software, so until a clear alternative exists, we can't deprecate
;;;; it, and even after it's been deprecated, we will support it for a few
;;;; years so everyone has time to migrate away from it. -- fare 2009-12-01

(defun* run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (asdf-message "; $ ~A~%" command)

    #+abcl
    (ext:run-shell-command command :output *verbose-out*)

    #+allegro
    ;; will this fail if command has embedded quotes - it seems to work
    (multiple-value-bind (stdout stderr exit-code)
        (excl.osi:command-output
         (format nil "~a -c \"~a\""
                 #+mswindows "sh" #-mswindows "/bin/sh" command)
         :input nil :whole nil
         #+mswindows :show-window #+mswindows :hide)
      (format *verbose-out* "~{~&; ~a~%~}~%" stderr)
      (format *verbose-out* "~{~&; ~a~%~}~%" stdout)
      exit-code)

    #+clisp                     ;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+clozure
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output *verbose-out*
                                 :wait t)))

    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (si:system command)

    #+gcl
    (lisp:system command)

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :show-cmd nil
     :prefix ""
     :output-stream *verbose-out*)

    #+sbcl
    (sb-ext:process-exit-code
     (apply #'sb-ext:run-program
            #+win32 "sh" #-win32 "/bin/sh"
            (list  "-c" command)
            :input nil :output *verbose-out*
            #+win32 '(:search t) #-win32 nil))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))

    #-(or abcl allegro clisp clozure cmu ecl gcl lispworks sbcl scl)
    (error "RUN-SHELL-COMMAND not implemented for this Lisp")))

;;;; ---------------------------------------------------------------------------
;;;; system-relative-pathname

(defmethod system-source-file ((system-name string))
  (system-source-file (find-system system-name)))
(defmethod system-source-file ((system-name symbol))
  (system-source-file (find-system system-name)))

(defun* system-source-directory (system-designator)
  "Return a pathname object corresponding to the
directory in which the system specification (.asd file) is
located."
     (make-pathname :name nil
                 :type nil
                 :defaults (system-source-file system-designator)))

(defun* relativize-directory (directory)
  (cond
    ((stringp directory)
     (list :relative directory))
    ((eq (car directory) :absolute)
     (cons :relative (cdr directory)))
    (t
     directory)))

(defun* relativize-pathname-directory (pathspec)
  (let ((p (pathname pathspec)))
    (make-pathname
     :directory (relativize-directory (pathname-directory p))
     :defaults p)))

(defun* system-relative-pathname (system name &key type)
  (merge-pathnames*
   (merge-component-name-type name :type type)
   (system-source-directory system)))


;;; ---------------------------------------------------------------------------
;;; implementation-identifier
;;;
;;; produce a string to identify current implementation.
;;; Initially stolen from SLIME's SWANK, hacked since.

(defparameter *implementation-features*
  '((:acl :allegro)
    (:lw :lispworks)
    (:digitool) ; before clozure, so it won't get preempted by ccl
    (:ccl :clozure)
    (:corman :cormanlisp)
    (:abcl :armedbear)
    :sbcl :cmu :clisp :gcl :ecl :scl))

(defparameter *os-features*
  '((:win :windows :mswindows :win32 :mingw32) ;; shorten things on windows
    (:solaris :sunos)
    (:linux :linux-target) ;; for GCL at least, must appear before :bsd.
    (:macosx :darwin :darwin-target :apple)
    :freebsd :netbsd :openbsd :bsd
    :unix))

(defparameter *architecture-features*
  '((:amd64 :x86-64 :x86_64 :x8664-target)
    (:x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
    :hppa64
    :hppa
    (:ppc64 :ppc64-target)
    (:ppc32 :ppc32-target :ppc :powerpc)
    :sparc64
    (:sparc32 :sparc)
    (:arm :arm-target)
    (:java :java-1.4 :java-1.5 :java-1.6 :java-1.7)))

(defun* lisp-version-string ()
  (let ((s (lisp-implementation-version)))
    (declare (ignorable s))
    #+allegro (format nil
                      "~A~A~A~A"
                      excl::*common-lisp-version-number*
                      ;; ANSI vs MoDeRn - thanks to Robert Goldman and Charley Cox
                      (if (eq excl:*current-case-mode*
                              :case-sensitive-lower) "M" "A")
                      ;; Note if not using International ACL
                      ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
                      (excl:ics-target-case
                       (:-ics "8")
                       (:+ics ""))
                      (if (member :64bit *features*) "-64bit" ""))
    #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
    #+clisp (subseq s 0 (position #\space s))
    #+clozure (format nil "~d.~d-f~d" ; shorten for windows
                      ccl::*openmcl-major-version*
                      ccl::*openmcl-minor-version*
                      (logand ccl::fasl-version #xFF))
    #+cmu (substitute #\- #\/ s)
    #+digitool (subseq s 8)
    #+ecl (format nil "~A~@[-~A~]" s
                  (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                    (when (>= (length vcs-id) 8)
                      (subseq vcs-id 0 8))))
    #+gcl (subseq s (1+ (position #\space s)))
    #+lispworks (format nil "~A~@[~A~]" s
                        (when (member :lispworks-64bit *features*) "-64bit"))
    ;; #+sbcl (format nil "~a-fasl~d" s sb-fasl:+fasl-file-version+) ; f-f-v redundant w/ version
    #+(or cormanlisp mcl sbcl scl) s
    #-(or allegro armedbear clisp clozure cmu cormanlisp digitool
          ecl gcl lispworks mcl sbcl scl) s))

(defun* first-feature (features)
  (labels
      ((fp (thing)
         (etypecase thing
           (symbol
            (let ((feature (find thing *features*)))
              (when feature (return-from fp feature))))
           ;; allows features to be lists of which the first
           ;; member is the "main name", the rest being aliases
           (cons
            (dolist (subf thing)
              (when (find subf *features*) (return-from fp (first thing))))))
         nil))
    (loop :for f :in features
      :when (fp f) :return :it)))

(defun* implementation-type ()
  (first-feature *implementation-features*))

(defun* implementation-identifier ()
  (labels
      ((maybe-warn (value fstring &rest args)
         (cond (value)
               (t (apply #'warn fstring args)
                  "unknown"))))
    (let ((lisp (maybe-warn (implementation-type)
                            "No implementation feature found in ~a."
                            *implementation-features*))
          (os   (maybe-warn (first-feature *os-features*)
                            "No os feature found in ~a." *os-features*))
          (arch (maybe-warn (first-feature *architecture-features*)
                            "No architecture feature found in ~a."
                            *architecture-features*))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp implementation version.")))
      (substitute-if
       #\_ (lambda (x) (find x " /:\\(){}[]$#`'\""))
       (format nil "~(~@{~a~^-~}~)" lisp version os arch)))))



;;; ---------------------------------------------------------------------------
;;; Generic support for configuration files

(defparameter *inter-directory-separator*
  #+(or unix cygwin) #\:
  #-(or unix cygwin) #\;)

(defun* user-homedir ()
  (truename (user-homedir-pathname)))

(defun* try-directory-subpath (x sub &key type)
  (let* ((p (and x (ensure-directory-pathname x)))
         (tp (and p (probe-file* p)))
         (sp (and tp (merge-pathnames* (merge-component-name-type sub :type type) p)))
         (ts (and sp (probe-file* sp))))
    (and ts (values sp ts))))
(defun* user-configuration-directories ()
  (remove-if
   #'null
   (flet ((try (x sub) (try-directory-subpath x sub :type :directory)))
     `(,(try (getenv "XDG_CONFIG_HOME") "common-lisp/")
       ,@(loop :with dirs = (getenv "XDG_CONFIG_DIRS")
           :for dir :in (split-string dirs :separator ":")
           :collect (try dir "common-lisp/"))
       #+(and (or win32 windows mswindows mingw32) (not cygwin))
        ,@`(#+lispworks ,(try (sys:get-folder-path :common-appdata) "common-lisp/config/")
            ;;; read-windows-registry HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\AppData
           ,(try (getenv "APPDATA") "common-lisp/config/"))
       ,(try (user-homedir) ".config/common-lisp/")))))
(defun* system-configuration-directories ()
  (remove-if
   #'null
   (append
    #+(and (or win32 windows mswindows mingw32) (not cygwin))
    (flet ((try (x sub) (try-directory-subpath x sub :type :directory)))
      `(,@`(#+lispworks ,(try (sys:get-folder-path :local-appdata) "common-lisp/config/")
           ;;; read-windows-registry HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\Common AppData
        ,(try (getenv "ALLUSERSPROFILE") "Application Data/common-lisp/config/"))))
    (list #p"/etc/common-lisp/"))))
(defun* in-first-directory (dirs x)
  (loop :for dir :in dirs
    :thereis (and dir (probe-file* (merge-pathnames* x (ensure-directory-pathname dir))))))
(defun* in-user-configuration-directory (x)
  (in-first-directory (user-configuration-directories) x))
(defun* in-system-configuration-directory (x)
  (in-first-directory (system-configuration-directories) x))

(defun* configuration-inheritance-directive-p (x)
  (let ((kw '(:inherit-configuration :ignore-inherited-configuration)))
    (or (member x kw)
        (and (length=n-p x 1) (member (car x) kw)))))

(defun* validate-configuration-form (form tag directive-validator
                                    &optional (description tag))
  (unless (and (consp form) (eq (car form) tag))
    (error "Error: Form doesn't specify ~A ~S~%" description form))
  (loop :with inherit = 0
    :for directive :in (cdr form) :do
    (if (configuration-inheritance-directive-p directive)
        (incf inherit)
        (funcall directive-validator directive))
    :finally
    (unless (= inherit 1)
      (error "One and only one of ~S or ~S is required"
             :inherit-configuration :ignore-inherited-configuration)))
  form)

(defun* validate-configuration-file (file validator description)
  (let ((forms (read-file-forms file)))
    (unless (length=n-p forms 1)
      (error "One and only one form allowed for ~A. Got: ~S~%" description forms))
    (funcall validator (car forms))))

(defun* hidden-file-p (pathname)
  (equal (first-char (pathname-name pathname)) #\.))

(defun* validate-configuration-directory (directory tag validator)
  (let ((files (sort (ignore-errors
                       (remove-if
                        'hidden-file-p
                        (directory (make-pathname :name :wild :type "conf" :defaults directory)
                                   #+sbcl :resolve-symlinks #+sbcl nil)))
                     #'string< :key #'namestring)))
    `(,tag
      ,@(loop :for file :in files :append
          (mapcar validator (read-file-forms file)))
      :inherit-configuration)))


;;; ---------------------------------------------------------------------------
;;; asdf-output-translations
;;;
;;; this code is heavily inspired from
;;; asdf-binary-translations, common-lisp-controller and cl-launch.
;;; ---------------------------------------------------------------------------

(defvar *output-translations* ()
  "Either NIL (for uninitialized), or a list of one element,
said element itself being a sorted list of mappings.
Each mapping is a pair of a source pathname and destination pathname,
and the order is by decreasing length of namestring of the source pathname.")

(defvar *user-cache*
  (flet ((try (x &rest sub) (and x `(,x ,@sub))))
    (or
     (try (getenv "XDG_CACHE_HOME") "common-lisp" :implementation)
     #+(and (or win32 windows mswindows mingw32) (not cygwin))
     (try (getenv "APPDATA") "common-lisp" "cache" :implementation)
     '(:home ".cache" "common-lisp" :implementation))))
(defvar *system-cache*
  ;; No good default, plus there's a security problem
  ;; with other users messing with such directories.
  *user-cache*)

(defun* output-translations ()
  (car *output-translations*))

(defun* (setf output-translations) (new-value)
  (setf *output-translations*
        (list
         (stable-sort (copy-list new-value) #'>
                      :key (lambda (x)
                             (etypecase (car x)
                               ((eql t) -1)
                               (pathname
                                (length (pathname-directory (car x)))))))))
  new-value)

(defun* output-translations-initialized-p ()
  (and *output-translations* t))

(defun* clear-output-translations ()
  "Undoes any initialization of the output translations.
You might want to call that before you dump an image that would be resumed
with a different configuration, so the configuration would be re-read then."
  (setf *output-translations* '())
  (values))

(declaim (ftype (function (t &key (:directory boolean) (:wilden boolean))
                          (values (or null pathname) &optional))
                resolve-location))

(defun* resolve-relative-location-component (super x &key directory wilden)
  (let* ((r (etypecase x
              (pathname x)
              (string x)
              (cons
               (return-from resolve-relative-location-component
                 (if (null (cdr x))
                     (resolve-relative-location-component
                      super (car x) :directory directory :wilden wilden)
                     (let* ((car (resolve-relative-location-component
                                  super (car x) :directory t :wilden nil))
                            (cdr (resolve-relative-location-component
                                  (merge-pathnames* car super) (cdr x)
                                  :directory directory :wilden wilden)))
                       (merge-pathnames* cdr car)))))
              ((eql :default-directory)
               (relativize-pathname-directory (default-directory)))
              ((eql :implementation) (implementation-identifier))
              ((eql :implementation-type) (string-downcase (implementation-type)))
              #-(and (or win32 windows mswindows mingw32) (not cygwin))
              ((eql :uid) (princ-to-string (get-uid)))))
         (d (if (or (pathnamep x) (not directory)) r (ensure-directory-pathname r)))
         (s (if (or (pathnamep x) (not wilden)) d (wilden d))))
    (when (and (absolute-pathname-p s) (not (pathname-match-p s (wilden super))))
      (error "pathname ~S is not relative to ~S" s super))
    (merge-pathnames* s super)))

(defun* resolve-absolute-location-component (x &key directory wilden)
  (let* ((r
          (etypecase x
            (pathname x)
            (string (if directory (ensure-directory-pathname x) (parse-namestring x)))
            (cons
             (return-from resolve-absolute-location-component
               (if (null (cdr x))
                   (resolve-absolute-location-component
                    (car x) :directory directory :wilden wilden)
                   (let* ((car (resolve-absolute-location-component
                                (car x) :directory t :wilden nil))
                          (cdr (resolve-relative-location-component
                                car (cdr x) :directory directory :wilden wilden)))
                     (merge-pathnames* cdr car))))) ; XXX why is this not just "cdr" ?
            ((eql :root)
             ;; special magic! we encode such paths as relative pathnames,
             ;; but it means "relative to the root of the source pathname's host and device".
             (return-from resolve-absolute-location-component
               (let ((p (make-pathname :directory '(:relative))))
                 (if wilden (wilden p) p))))
            ((eql :home) (user-homedir))
            ((eql :user-cache) (resolve-location *user-cache* :directory t :wilden nil))
            ((eql :system-cache) (resolve-location *system-cache* :directory t :wilden nil))
            ((eql :default-directory) (default-directory))))
         (s (if (and wilden (not (pathnamep x)))
                (wilden r)
                r)))
    (unless (absolute-pathname-p s)
      (error "Not an absolute pathname ~S" s))
    s))

(defun* resolve-location (x &key directory wilden)
  (if (atom x)
      (resolve-absolute-location-component x :directory directory :wilden wilden)
      (loop :with path = (resolve-absolute-location-component
                          (car x) :directory (and (or directory (cdr x)) t)
                          :wilden (and wilden (null (cdr x))))
        :for (component . morep) :on (cdr x)
        :for dir = (and (or morep directory) t)
        :for wild = (and wilden (not morep))
        :do (setf path (resolve-relative-location-component
                        path component :directory dir :wilden wild))
        :finally (return path))))

(defun* location-designator-p (x)
  (flet ((componentp (c) (typep c '(or string pathname keyword))))
    (or (typep x 'boolean) (componentp x) (and (consp x) (every #'componentp x)))))

(defun* location-function-p (x)
  (and
   (consp x)
   (length=n-p x 2)
   (or (and (equal (first x) :function)
            (typep (second x) 'symbol))
       (and (equal (first x) 'lambda)
            (cddr x)
            (length=n-p (second x) 2)))))

(defun* validate-output-translations-directive (directive)
  (unless
      (or (member directive '(:inherit-configuration
                              :ignore-inherited-configuration
                              :enable-user-cache :disable-cache nil))
          (and (consp directive)
               (or (and (length=n-p directive 2)
                        (or (and (eq (first directive) :include)
                                 (typep (second directive) '(or string pathname null)))
                            (and (location-designator-p (first directive))
                                 (or (location-designator-p (second directive))
                                     (location-function-p (second directive))))))
                   (and (length=n-p directive 1)
                        (location-designator-p (first directive))))))
    (error "Invalid directive ~S~%" directive))
  directive)

(defun* validate-output-translations-form (form)
  (validate-configuration-form
   form
   :output-translations
   'validate-output-translations-directive
   "output translations"))

(defun* validate-output-translations-file (file)
  (validate-configuration-file
   file 'validate-output-translations-form "output translations"))

(defun* validate-output-translations-directory (directory)
  (validate-configuration-directory
   directory :output-translations 'validate-output-translations-directive))

(defun* parse-output-translations-string (string)
  (cond
    ((or (null string) (equal string ""))
     '(:output-translations :inherit-configuration))
    ((not (stringp string))
     (error "environment string isn't: ~S" string))
    ((eql (char string 0) #\")
     (parse-output-translations-string (read-from-string string)))
    ((eql (char string 0) #\()
     (validate-output-translations-form (read-from-string string)))
    (t
     (loop
      :with inherit = nil
      :with directives = ()
      :with start = 0
      :with end = (length string)
      :with source = nil
      :for i = (or (position *inter-directory-separator* string :start start) end) :do
      (let ((s (subseq string start i)))
        (cond
          (source
           (push (list source (if (equal "" s) nil s)) directives)
           (setf source nil))
          ((equal "" s)
           (when inherit
             (error "only one inherited configuration allowed: ~S" string))
           (setf inherit t)
           (push :inherit-configuration directives))
          (t
           (setf source s)))
        (setf start (1+ i))
        (when (> start end)
          (when source
            (error "Uneven number of components in source to destination mapping ~S" string))
          (unless inherit
            (push :ignore-inherited-configuration directives))
          (return `(:output-translations ,@(nreverse directives)))))))))

(defparameter *default-output-translations*
  '(environment-output-translations
    user-output-translations-pathname
    user-output-translations-directory-pathname
    system-output-translations-pathname
    system-output-translations-directory-pathname))

(defun* wrapping-output-translations ()
  `(:output-translations
    ;; Some implementations have precompiled ASDF systems,
    ;; so we must disable translations for implementation paths.
    #+sbcl ,(let ((h (getenv "SBCL_HOME"))) (when (plusp (length h)) `(,h ())))
    #+ecl (,(translate-logical-pathname "SYS:**;*.*") ()) ; not needed: no precompiled ASDF system
    #+clozure ,(ignore-errors (list (wilden (let ((*default-pathname-defaults* #p"")) (truename #p"ccl:"))) ())) ; not needed: no precompiled ASDF system
    ;; All-import, here is where we want user stuff to be:
    :inherit-configuration
    ;; These are for convenience, and can be overridden by the user:
    #+abcl (#p"/___jar___file___root___/**/*.*" (:user-cache #p"**/*.*"))
    #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
    ;; We enable the user cache by default, and here is the place we do:
    :enable-user-cache))

(defparameter *output-translations-file* #p"asdf-output-translations.conf")
(defparameter *output-translations-directory* #p"asdf-output-translations.conf.d/")

(defun* user-output-translations-pathname ()
  (in-user-configuration-directory *output-translations-file* ))
(defun* system-output-translations-pathname ()
  (in-system-configuration-directory *output-translations-file*))
(defun* user-output-translations-directory-pathname ()
  (in-user-configuration-directory *output-translations-directory*))
(defun* system-output-translations-directory-pathname ()
  (in-system-configuration-directory *output-translations-directory*))
(defun* environment-output-translations ()
  (getenv "ASDF_OUTPUT_TRANSLATIONS"))

(defgeneric* process-output-translations (spec &key inherit collect))
(declaim (ftype (function (t &key (:collect (or symbol function))) t)
                inherit-output-translations))
(declaim (ftype (function (t &key (:collect (or symbol function)) (:inherit list)) t)
                process-output-translations-directive))

(defmethod process-output-translations ((x symbol) &key
                                        (inherit *default-output-translations*)
                                        collect)
  (process-output-translations (funcall x) :inherit inherit :collect collect))
(defmethod process-output-translations ((pathname pathname) &key inherit collect)
  (cond
    ((directory-pathname-p pathname)
     (process-output-translations (validate-output-translations-directory pathname)
                                  :inherit inherit :collect collect))
    ((probe-file pathname)
     (process-output-translations (validate-output-translations-file pathname)
                                  :inherit inherit :collect collect))
    (t
     (inherit-output-translations inherit :collect collect))))
(defmethod process-output-translations ((string string) &key inherit collect)
  (process-output-translations (parse-output-translations-string string)
                               :inherit inherit :collect collect))
(defmethod process-output-translations ((x null) &key inherit collect)
  (declare (ignorable x))
  (inherit-output-translations inherit :collect collect))
(defmethod process-output-translations ((form cons) &key inherit collect)
  (dolist (directive (cdr (validate-output-translations-form form)))
    (process-output-translations-directive directive :inherit inherit :collect collect)))

(defun* inherit-output-translations (inherit &key collect)
  (when inherit
    (process-output-translations (first inherit) :collect collect :inherit (rest inherit))))

(defun* process-output-translations-directive (directive &key inherit collect)
  (if (atom directive)
      (ecase directive
        ((:enable-user-cache)
         (process-output-translations-directive '(t :user-cache) :collect collect))
        ((:disable-cache)
         (process-output-translations-directive '(t t) :collect collect))
        ((:inherit-configuration)
         (inherit-output-translations inherit :collect collect))
        ((:ignore-inherited-configuration nil)
         nil))
      (let ((src (first directive))
            (dst (second directive)))
        (if (eq src :include)
            (when dst
              (process-output-translations (pathname dst) :inherit nil :collect collect))
            (when src
              (let ((trusrc (or (eql src t)
                                (let ((loc (resolve-location src :directory t :wilden t)))
                                  (if (absolute-pathname-p loc) (truenamize loc) loc)))))
                (cond
                  ((location-function-p dst)
                   (funcall collect
                            (list trusrc
                                  (if (symbolp (second dst))
                                      (fdefinition (second dst))
                                      (eval (second dst))))))
                  ((eq dst t)
                   (funcall collect (list trusrc t)))
                  (t
                   (let* ((trudst (make-pathname
                                   :defaults (if dst (resolve-location dst :directory t :wilden t) trusrc)))
                          (wilddst (make-pathname
                                    :name :wild :type :wild :version :wild
                                    :defaults trudst)))
                     (funcall collect (list wilddst t))
                     (funcall collect (list trusrc trudst)))))))))))

(defun* compute-output-translations (&optional parameter)
  "read the configuration, return it"
  (remove-duplicates
   (while-collecting (c)
     (inherit-output-translations
      `(wrapping-output-translations ,parameter ,@*default-output-translations*) :collect #'c))
   :test 'equal :from-end t))

(defun* initialize-output-translations (&optional parameter)
  "read the configuration, initialize the internal configuration variable,
return the configuration"
  (setf (output-translations) (compute-output-translations parameter)))

(defun* disable-output-translations ()
  "Initialize output translations in a way that maps every file to itself,
effectively disabling the output translation facility."
  (initialize-output-translations
   '(:output-translations :disable-cache :ignore-inherited-configuration)))

;; checks an initial variable to see whether the state is initialized
;; or cleared. In the former case, return current configuration; in
;; the latter, initialize.  ASDF will call this function at the start
;; of (asdf:find-system).
(defun* ensure-output-translations ()
  (if (output-translations-initialized-p)
      (output-translations)
      (initialize-output-translations)))

(defun* translate-pathname* (path absolute-source destination &optional root source)
  (declare (ignore source))
  (cond
    ((functionp destination)
     (funcall destination path absolute-source))
    ((eq destination t)
     path)
    ((not (pathnamep destination))
     (error "invalid destination"))
    ((not (absolute-pathname-p destination))
     (translate-pathname path absolute-source (merge-pathnames* destination root)))
    (root
     (translate-pathname (directorize-pathname-host-device path) absolute-source destination))
    (t
     (translate-pathname path absolute-source destination))))

(defun* apply-output-translations (path)
  (etypecase path
    (logical-pathname
     path)
    ((or pathname string)
     (ensure-output-translations)
     (loop :with p = (truenamize path)
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

(defmethod output-files :around (operation component)
  "Translate output files, unless asked not to"
  (declare (ignorable operation component))
  (values
   (multiple-value-bind (files fixedp) (call-next-method)
     (if fixedp
         files
         (mapcar #'apply-output-translations files)))
   t))

(defun* compile-file-pathname* (input-file &rest keys &key output-file &allow-other-keys)
  (or output-file
      (apply-output-translations
       (apply 'compile-file-pathname
              (truenamize (lispize-pathname input-file))
              keys))))

(defun* tmpize-pathname (x)
  (make-pathname
   :name (format nil "ASDF-TMP-~A" (pathname-name x))
   :defaults x))

(defun* delete-file-if-exists (x)
  (when (and x (probe-file x))
    (delete-file x)))

(defun* compile-file* (input-file &rest keys &key output-file &allow-other-keys)
  (let* ((output-file (or output-file (apply 'compile-file-pathname* input-file keys)))
         (tmp-file (tmpize-pathname output-file))
         (status :error))
    (multiple-value-bind (output-truename warnings-p failure-p)
        (apply 'compile-file input-file :output-file tmp-file keys)
      (cond
        (failure-p
         (setf status *compile-file-failure-behaviour*))
        (warnings-p
         (setf status *compile-file-warnings-behaviour*))
        (t
         (setf status :success)))
      (ecase status
        ((:success :warn :ignore)
         (delete-file-if-exists output-file)
         (when output-truename
           (rename-file output-truename output-file)
           (setf output-truename output-file)))
        (:error
         (delete-file-if-exists output-truename)
         (setf output-truename nil)))
      (values output-truename warnings-p failure-p))))

#+abcl
(defun* translate-jar-pathname (source wildcard)
  (declare (ignore wildcard))
  (let* ((p (pathname (first (pathname-device source))))
         (root (format nil "/___jar___file___root___/~@[~A/~]"
                       (and (find :windows *features*)
                            (pathname-device p)))))
    (apply-output-translations
     (merge-pathnames*
      (relativize-pathname-directory source)
      (merge-pathnames*
       (relativize-pathname-directory (ensure-directory-pathname p))
       root)))))

;;;; -----------------------------------------------------------------
;;;; Compatibility mode for ASDF-Binary-Locations

(defun* enable-asdf-binary-locations-compatibility
    (&key
     (centralize-lisp-binaries nil)
     (default-toplevel-directory
         ;; Use ".cache/common-lisp" instead ???
         (merge-pathnames* (make-pathname :directory '(:relative ".fasls"))
                           (user-homedir)))
     (include-per-user-information nil)
     (map-all-source-files (or #+(or ecl clisp) t nil))
     (source-to-target-mappings nil))
  #+(or ecl clisp)
  (when (null map-all-source-files)
    (error "asdf:enable-asdf-binary-locations-compatibility doesn't support :map-all-source-files nil on ECL and CLISP"))
  (let* ((fasl-type (pathname-type (compile-file-pathname "foo.lisp")))
         (wild-inferiors (make-pathname :directory '(:relative :wild-inferiors)))
         (mapped-files (make-pathname
                        :name :wild :version :wild
                        :type (if map-all-source-files :wild fasl-type)))
         (destination-directory
          (if centralize-lisp-binaries
              `(,default-toplevel-directory
                ,@(when include-per-user-information
                        (cdr (pathname-directory (user-homedir))))
                :implementation ,wild-inferiors)
              `(:root ,wild-inferiors :implementation))))
    (initialize-output-translations
     `(:output-translations
       ,@source-to-target-mappings
       ((:root ,wild-inferiors ,mapped-files)
        (,@destination-directory ,mapped-files))
       (t t)
       :ignore-inherited-configuration))))

;;;; -----------------------------------------------------------------
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13

#+(and (or win32 windows mswindows mingw32) (not cygwin) (not clisp))
(progn
(defparameter *link-initial-dword* 76)
(defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

(defun* read-null-terminated-string (s)
  (with-output-to-string (out)
    (loop :for code = (read-byte s)
      :until (zerop code)
      :do (write-char (code-char code) out))))

(defun* read-little-endian (s &optional (bytes 4))
  (loop
    :for i :from 0 :below bytes
    :sum (ash (read-byte s) (* 8 i))))

(defun* parse-file-location-info (s)
  (let ((start (file-position s))
        (total-length (read-little-endian s))
        (end-of-header (read-little-endian s))
        (fli-flags (read-little-endian s))
        (local-volume-offset (read-little-endian s))
        (local-offset (read-little-endian s))
        (network-volume-offset (read-little-endian s))
        (remaining-offset (read-little-endian s)))
    (declare (ignore total-length end-of-header local-volume-offset))
    (unless (zerop fli-flags)
      (cond
        ((logbitp 0 fli-flags)
          (file-position s (+ start local-offset)))
        ((logbitp 1 fli-flags)
          (file-position s (+ start
                              network-volume-offset
                              #x14))))
      (concatenate 'string
        (read-null-terminated-string s)
        (progn
          (file-position s (+ start remaining-offset))
          (read-null-terminated-string s))))))

(defun* parse-windows-shortcut (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (handler-case
        (when (and (= (read-little-endian s) *link-initial-dword*)
                   (let ((header (make-array (length *link-guid*))))
                     (read-sequence header s)
                     (equalp header *link-guid*)))
          (let ((flags (read-little-endian s)))
            (file-position s 76)        ;skip rest of header
            (when (logbitp 0 flags)
              ;; skip shell item id list
              (let ((length (read-little-endian s 2)))
                (file-position s (+ length (file-position s)))))
            (cond
              ((logbitp 1 flags)
                (parse-file-location-info s))
              (t
                (when (logbitp 2 flags)
                  ;; skip description string
                  (let ((length (read-little-endian s 2)))
                    (file-position s (+ length (file-position s)))))
                (when (logbitp 3 flags)
                  ;; finally, our pathname
                  (let* ((length (read-little-endian s 2))
                         (buffer (make-array length)))
                    (read-sequence buffer s)
                    (map 'string #'code-char buffer)))))))
      (end-of-file ()
        nil)))))

;;;; -----------------------------------------------------------------
;;;; Source Registry Configuration, by Francois-Rene Rideau
;;;; See the Manual and https://bugs.launchpad.net/asdf/+bug/485918

;; Using ack 1.2 exclusions
(defvar *default-source-registry-exclusions*
  '(".bzr" ".cdv"
    ;; "~.dep" "~.dot" "~.nib" "~.plst" ; we don't support ack wildcards
    ".git" ".hg" ".pc" ".svn" "CVS" "RCS" "SCCS" "_darcs"
    "_sgbak" "autom4te.cache" "cover_db" "_build"
    "debian")) ;; debian often build stuff under the debian directory... BAD.

(defvar *source-registry-exclusions* *default-source-registry-exclusions*)

(defvar *source-registry* ()
  "Either NIL (for uninitialized), or a list of one element,
said element itself being a list of directory pathnames where to look for .asd files")

(defun* source-registry ()
  (car *source-registry*))

(defun* (setf source-registry) (new-value)
  (setf *source-registry* (list new-value))
  new-value)

(defun* source-registry-initialized-p ()
  (and *source-registry* t))

(defun* clear-source-registry ()
  "Undoes any initialization of the source registry.
You might want to call that before you dump an image that would be resumed
with a different configuration, so the configuration would be re-read then."
  (setf *source-registry* '())
  (values))

(defparameter *wild-asd*
  (make-pathname :directory nil :name :wild :type "asd" :version :newest))

(defun directory-has-asd-files-p (directory)
  (and (ignore-errors
         (directory (merge-pathnames* *wild-asd* directory)
                    #+sbcl #+sbcl :resolve-symlinks nil
                    #+ccl #+ccl :follow-links nil
                    #+clisp #+clisp :circle t))
       t))

(defun subdirectories (directory)
  (let* ((directory (ensure-directory-pathname directory))
         #-cormanlisp
         (wild (merge-pathnames*
                #-(or abcl allegro lispworks scl)
                (make-pathname :directory '(:relative :wild) :name nil :type nil :version nil)
                #+(or abcl allegro lispworks scl) "*.*"
                directory))
         (dirs
          #-cormanlisp
          (ignore-errors
            (directory wild .
              #.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                    #+ccl '(:follow-links nil :directories t :files nil)
                    #+clisp '(:circle t :if-does-not-exist :ignore)
                    #+(or cmu scl) '(:follow-links nil :truenamep nil)
                    #+digitool '(:directories t)
                    #+sbcl '(:resolve-symlinks nil))))
          #+cormanlisp (cl::directory-subdirs directory))
         #+(or abcl allegro lispworks scl)
         (dirs (remove-if-not #+abcl #'extensions:probe-directory
                              #+allegro #'excl:probe-directory
                              #+lispworks #'lw:file-directory-p
                              #-(or abcl allegro lispworks) #'directory-pathname-p
                              dirs)))
    dirs))

(defun collect-sub*directories (directory collectp recursep collector)
  (when (funcall collectp directory)
    (funcall collector directory))
  (dolist (subdir (subdirectories directory))
    (when (funcall recursep subdir)
      (collect-sub*directories subdir collectp recursep collector))))

(defun collect-sub*directories-with-asd
    (directory &key
     (exclude *default-source-registry-exclusions*)
     collect)
  (collect-sub*directories
   directory
   #'directory-has-asd-files-p
   #'(lambda (x) (not (member (car (last (pathname-directory x))) exclude :test #'equal)))
   collect))

(defun* validate-source-registry-directive (directive)
  (unless
      (or (member directive '(:default-registry (:default-registry)) :test 'equal)
          (destructuring-bind (kw &rest rest) directive
            (case kw
              ((:include :directory :tree)
               (and (length=n-p rest 1)
                    (location-designator-p (first rest))))
              ((:exclude :also-exclude)
               (every #'stringp rest))
              (null rest))))
    (error "Invalid directive ~S~%" directive))
  directive)

(defun* validate-source-registry-form (form)
  (validate-configuration-form
   form :source-registry 'validate-source-registry-directive "a source registry"))

(defun* validate-source-registry-file (file)
  (validate-configuration-file
   file 'validate-source-registry-form "a source registry"))

(defun* validate-source-registry-directory (directory)
  (validate-configuration-directory
   directory :source-registry 'validate-source-registry-directive))

(defun* parse-source-registry-string (string)
  (cond
    ((or (null string) (equal string ""))
     '(:source-registry :inherit-configuration))
    ((not (stringp string))
     (error "environment string isn't: ~S" string))
    ((find (char string 0) "\"(")
     (validate-source-registry-form (read-from-string string)))
    (t
     (loop
      :with inherit = nil
      :with directives = ()
      :with start = 0
      :with end = (length string)
      :for pos = (position *inter-directory-separator* string :start start) :do
      (let ((s (subseq string start (or pos end))))
        (cond
         ((equal "" s) ; empty element: inherit
          (when inherit
            (error "only one inherited configuration allowed: ~S" string))
          (setf inherit t)
          (push ':inherit-configuration directives))
         ((ends-with s "//")
          (push `(:tree ,(subseq s 0 (1- (length s)))) directives))
         (t
          (push `(:directory ,s) directives)))
        (cond
          (pos
           (setf start (1+ pos)))
          (t
           (unless inherit
             (push '(:ignore-inherited-configuration) directives))
           (return `(:source-registry ,@(nreverse directives))))))))))

(defun* register-asd-directory (directory &key recurse exclude collect)
  (if (not recurse)
      (funcall collect directory)
      (collect-sub*directories-with-asd
       directory :exclude exclude :collect collect)))

(defparameter *default-source-registries*
  '(environment-source-registry
    user-source-registry
    user-source-registry-directory
    system-source-registry
    system-source-registry-directory
    default-source-registry))

(defparameter *source-registry-file* #p"source-registry.conf")
(defparameter *source-registry-directory* #p"source-registry.conf.d/")

(defun* wrapping-source-registry ()
  `(:source-registry
    #+sbcl (:tree ,(getenv "SBCL_HOME"))
    :inherit-configuration
    #+cmu (:tree #p"modules:")))
(defun* default-source-registry ()
  (flet ((try (x sub) (try-directory-subpath x sub :type :directory)))
    `(:source-registry
      #+sbcl (:directory ,(merge-pathnames* ".sbcl/systems/" (user-homedir)))
      (:directory ,(truenamize (directory-namestring *default-pathname-defaults*)))
      ,@(let*
         #+(or unix cygwin)
         ((datahome
           (or (getenv "XDG_DATA_HOME")
               (try (user-homedir) ".local/share/")))
          (datadirs
           (or (getenv "XDG_DATA_DIRS") "/usr/local/share:/usr/share"))
          (dirs (cons datahome (split-string datadirs :separator ":"))))
         #+(and (or win32 windows mswindows mingw32) (not cygwin))
         ((datahome (getenv "APPDATA"))
          (datadir
           #+lispworks (sys:get-folder-path :local-appdata)
           #-lispworks (try (getenv "ALLUSERSPROFILE")
                            "Application Data"))
          (dirs (list datahome datadir)))
         #-(or unix win32 windows mswindows mingw32 cygwin)
         ((dirs ()))
         (loop :for dir :in dirs
           :collect `(:directory ,(try dir "common-lisp/systems/"))
           :collect `(:tree ,(try dir "common-lisp/source/"))))
      :inherit-configuration)))
(defun* user-source-registry ()
  (in-user-configuration-directory *source-registry-file*))
(defun* system-source-registry ()
  (in-system-configuration-directory *source-registry-file*))
(defun* user-source-registry-directory ()
  (in-user-configuration-directory *source-registry-directory*))
(defun* system-source-registry-directory ()
  (in-system-configuration-directory *source-registry-directory*))
(defun* environment-source-registry ()
  (getenv "CL_SOURCE_REGISTRY"))

(defgeneric* process-source-registry (spec &key inherit register))
(declaim (ftype (function (t &key (:register (or symbol function))) t)
                inherit-source-registry))
(declaim (ftype (function (t &key (:register (or symbol function)) (:inherit list)) t)
                process-source-registry-directive))

(defmethod process-source-registry ((x symbol) &key inherit register)
  (process-source-registry (funcall x) :inherit inherit :register register))
(defmethod process-source-registry ((pathname pathname) &key inherit register)
  (cond
    ((directory-pathname-p pathname)
     (process-source-registry (validate-source-registry-directory pathname)
                              :inherit inherit :register register))
    ((probe-file pathname)
     (process-source-registry (validate-source-registry-file pathname)
                              :inherit inherit :register register))
    (t
     (inherit-source-registry inherit :register register))))
(defmethod process-source-registry ((string string) &key inherit register)
  (process-source-registry (parse-source-registry-string string)
                           :inherit inherit :register register))
(defmethod process-source-registry ((x null) &key inherit register)
  (declare (ignorable x))
  (inherit-source-registry inherit :register register))
(defmethod process-source-registry ((form cons) &key inherit register)
  (let ((*source-registry-exclusions* *default-source-registry-exclusions*))
    (dolist (directive (cdr (validate-source-registry-form form)))
      (process-source-registry-directive directive :inherit inherit :register register))))

(defun* inherit-source-registry (inherit &key register)
  (when inherit
    (process-source-registry (first inherit) :register register :inherit (rest inherit))))

(defun* process-source-registry-directive (directive &key inherit register)
  (destructuring-bind (kw &rest rest) (if (consp directive) directive (list directive))
    (ecase kw
      ((:include)
       (destructuring-bind (pathname) rest
         (process-source-registry (resolve-location pathname) :inherit nil :register register)))
      ((:directory)
       (destructuring-bind (pathname) rest
         (when pathname
           (funcall register (resolve-location pathname :directory t)))))
      ((:tree)
       (destructuring-bind (pathname) rest
         (when pathname
           (funcall register (resolve-location pathname :directory t)
                    :recurse t :exclude *source-registry-exclusions*))))
      ((:exclude)
       (setf *source-registry-exclusions* rest))
      ((:also-exclude)
       (appendf *source-registry-exclusions* rest))
      ((:default-registry)
       (inherit-source-registry '(default-source-registry) :register register))
      ((:inherit-configuration)
       (inherit-source-registry inherit :register register))
      ((:ignore-inherited-configuration)
       nil)))
  nil)

(defun* flatten-source-registry (&optional parameter)
  (remove-duplicates
   (while-collecting (collect)
     (inherit-source-registry
      `(wrapping-source-registry
        ,parameter
        ,@*default-source-registries*)
      :register (lambda (directory &key recurse exclude)
                  (collect (list directory :recurse recurse :exclude exclude)))))
   :test 'equal :from-end t))

;; Will read the configuration and initialize all internal variables,
;; and return the new configuration.
(defun* compute-source-registry (&optional parameter)
  (while-collecting (collect)
    (dolist (entry (flatten-source-registry parameter))
      (destructuring-bind (directory &key recurse exclude) entry
        (register-asd-directory
         directory
         :recurse recurse :exclude exclude :collect #'collect)))))

(defun* initialize-source-registry (&optional parameter)
  (setf (source-registry) (compute-source-registry parameter)))

;; Checks an initial variable to see whether the state is initialized
;; or cleared. In the former case, return current configuration; in
;; the latter, initialize.  ASDF will call this function at the start
;; of (asdf:find-system) to make sure the source registry is initialized.
;; However, it will do so *without* a parameter, at which point it
;; will be too late to provide a parameter to this function, though
;; you may override the configuration explicitly by calling
;; initialize-source-registry directly with your parameter.
(defun* ensure-source-registry (&optional parameter)
  (if (source-registry-initialized-p)
      (source-registry)
      (initialize-source-registry parameter)))

(defun* sysdef-source-registry-search (system)
  (ensure-source-registry)
  (loop :with name = (coerce-name system)
    :for defaults :in (source-registry)
    :for file = (probe-asd name defaults)
    :when file :return file))

(defun* clear-configuration ()
  (clear-source-registry)
  (clear-output-translations))

;;;; -----------------------------------------------------------------
;;;; Hook into REQUIRE for ABCL, ClozureCL, CMUCL, ECL and SBCL
;;;;
(defun* module-provide-asdf (name)
  (handler-bind
      ((style-warning #'muffle-warning)
       (missing-component (constantly nil))
       (error (lambda (e)
                (format *error-output* "ASDF could not load ~(~A~) because ~A.~%"
                        name e))))
    (let* ((*verbose-out* (make-broadcast-stream))
           (system (find-system (string-downcase name) nil)))
      (when system
        (load-system system)
        t))))

#+(or abcl clisp clozure cmu ecl sbcl)
(let ((x (and #+clisp (find-symbol "*MODULE-PROVIDER-FUNCTIONS*" :custom))))
  (when x
    (eval `(pushnew 'module-provide-asdf
            #+abcl sys::*module-provider-functions*
            #+clisp ,x
            #+clozure ccl:*module-provider-functions*
            #+cmu ext:*module-provider-functions*
            #+ecl si:*module-provider-functions*
            #+sbcl sb-ext:*module-provider-functions*))))


;;;; -------------------------------------------------------------------------
;;;; Cleanups after hot-upgrade.
;;;; Things to do in case we're upgrading from a previous version of ASDF.
;;;; See https://bugs.launchpad.net/asdf/+bug/485687
;;;;
;;;; TODO: debug why it's not enough to upgrade from ECL <= 9.11.1
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ecl ;; Support upgrade from before ECL went to 1.369
  (when (fboundp 'compile-op-system-p)
    (defmethod compile-op-system-p ((op compile-op))
      (getf :system-p (compile-op-flags op)))
    (defmethod initialize-instance :after ((op compile-op)
                                           &rest initargs
                                           &key system-p &allow-other-keys)
      (declare (ignorable initargs))
      (when system-p (appendf (compile-op-flags op) (list :system-p system-p))))))

;;;; -----------------------------------------------------------------
;;;; Done!
(when *load-verbose*
  (asdf-message ";; ASDF, version ~a~%" (asdf-version)))

#+allegro
(eval-when (:compile-toplevel :execute)
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* *acl-warn-save*)))

(pushnew :asdf *features*)
(pushnew :asdf2 *features*)

(provide :asdf)

;;; Local Variables:
;;; mode: lisp
;;; End:
