;;; This is asdf: Another System Definition Facility.  1.102
;;;
;;; Feedback, bug reports, and patches are all welcome: please mail to
;;; <cclan-list@lists.sf.net>.  But note first that the canonical
;;; source for asdf is presently the cCLan CVS repository at
;;; <URL:http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/cclan/asdf/>
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the CVS HEAD
;;; is the latest development version, whereas the revision tagged
;;; RELEASE may be slightly older but is considered `stable'

;;; Copyright (c) 2001-2003 Daniel Barlow and contributors
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

;;; the problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file

(defpackage #:asdf
  (:export #:defsystem #:oos #:operate #:find-system #:run-shell-command
           #:system-definition-pathname #:find-component ; miscellaneous
           #:hyperdocumentation #:hyperdoc

           #:compile-op #:load-op #:load-source-op #:test-system-version
           #:test-op
           #:operation                  ; operations
           #:feature                    ; sort-of operation
           #:version                    ; metaphorically sort-of an operation

           #:input-files #:output-files #:perform       ; operation methods
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

           #:operation-on-warnings
           #:operation-on-failure

           ;#:*component-parent-pathname*
           #:*system-definition-search-functions*
           #:*central-registry*         ; variables
           #:*compile-file-warnings-behaviour*
           #:*compile-file-failure-behaviour*
           #:*asdf-revision*

           #:operation-error #:compile-failed #:compile-warned #:compile-error
           #:error-component #:error-operation
           #:system-definition-error
           #:missing-component
           #:missing-dependency
           #:circular-dependency        ; errors
           #:duplicate-names

           #:retry
           #:accept                     ; restarts

           #:preference-file-for-system/operation
           #:load-preferences
           )
  (:use :cl))

#+nil
(error "The author of this file habitually uses #+nil to comment out forms.  But don't worry, it was unlikely to work in the New Implementation of Lisp anyway")


(in-package #:asdf)

(defvar *asdf-revision* (let* ((v "1.102")
                               (colon (or (position #\: v) -1))
                               (dot (position #\. v)))
                          (and v colon dot
                               (list (parse-integer v :start (1+ colon)
                                                    :junk-allowed t)
                                     (parse-integer v :start (1+ dot)
                                                    :junk-allowed t)))))

(defvar *compile-file-warnings-behaviour* :warn)
(defvar *compile-file-failure-behaviour* #+sbcl :error #-sbcl :warn)

(defvar *verbose-out* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility stuff

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

(define-modify-macro appendf (&rest args)
                     append "Append onto list")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes, condiitons

(define-condition system-definition-error (error) ()
  ;; [this use of :report should be redundant, but unfortunately it's not.
  ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
  ;; over print-object; this is always conditions::%print-condition for
  ;; condition objects, which in turn does inheritance of :report options at
  ;; run-time.  fortunately, inheritance means we only need this kludge here in
  ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
  #+cmu (:report print-object))

(define-condition formatted-system-definition-error (system-definition-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
             (apply #'format s (format-control c) (format-arguments c)))))

(define-condition circular-dependency (system-definition-error)
  ((components :initarg :components :reader circular-dependency-components)))

(define-condition duplicate-names (system-definition-error)
  ((name :initarg :name :reader duplicate-names-name)))

(define-condition missing-component (system-definition-error)
  ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
   (version :initform nil :reader missing-version :initarg :version)
   (parent :initform nil :reader missing-parent :initarg :parent)))

(define-condition missing-dependency (missing-component)
  ((required-by :initarg :required-by :reader missing-required-by)))

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
   (in-order-to :initform nil :initarg :in-order-to)
   ;;; XXX crap name
   (do-first :initform nil :initarg :do-first)
   ;; methods defined using the "inline" style inside a defsystem form:
   ;; need to store them somewhere so we can delete them when the system
   ;; is re-evaluated
   (inline-methods :accessor component-inline-methods :initform nil)
   (parent :initarg :parent :initform nil :reader component-parent)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (relative-pathname :initarg :pathname)
   (operation-times :initform (make-hash-table )
                    :accessor component-operation-times)
   ;; XXX we should provide some atomic interface for updating the
   ;; component properties
   (properties :accessor component-properties :initarg :properties
               :initform nil)))

;;;; methods: conditions

(defmethod print-object ((c missing-dependency) s)
  (format s "~@<~A, required by ~A~@:>"
          (call-next-method c nil) (missing-required-by c)))

(defun sysdef-error (format &rest arguments)
  (error 'formatted-system-definition-error :format-control format :format-arguments arguments))

;;;; methods: components

(defmethod print-object ((c missing-component) s)
  (format s "~@<component ~S not found~
             ~@[ or does not match version ~A~]~
             ~@[ in ~A~]~@:>"
          (missing-requires c)
          (missing-version c)
          (when (missing-parent c)
            (component-name (missing-parent c)))))

(defgeneric component-system (component)
  (:documentation "Find the top-level system containing COMPONENT"))

(defmethod component-system ((component component))
  (aif (component-parent component)
       (component-system it)
       component))

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (ignore-errors
      (prin1 (component-name c) stream))))

(defclass module (component)
  ((components :initform nil :accessor module-components :initarg :components)
   ;; what to do if we can't satisfy a dependency of one of this module's
   ;; components.  This allows a limited form of conditional processing
   (if-component-dep-fails :initform :fail
                           :accessor module-if-component-dep-fails
                           :initarg :if-component-dep-fails)
   (default-component-class :accessor module-default-component-class
     :initform 'cl-source-file :initarg :default-component-class)))

(defgeneric component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))

(defun component-parent-pathname (component)
  (aif (component-parent component)
       (component-pathname it)
       *default-pathname-defaults*))

(defgeneric component-relative-pathname (component)
  (:documentation "Extracts the relative pathname applicable for a particular component."))

(defmethod component-relative-pathname ((component module))
  (or (slot-value component 'relative-pathname)
      (make-pathname
       :directory `(:relative ,(component-name component))
       :host (pathname-host (component-parent-pathname component)))))

(defmethod component-pathname ((component component))
  (let ((*default-pathname-defaults* (component-parent-pathname component)))
    (merge-pathnames (component-relative-pathname component))))

(defgeneric component-property (component property))

(defmethod component-property ((c component) property)
  (cdr (assoc property (slot-value c 'properties) :test #'equal)))

(defgeneric (setf component-property) (new-value component property))

(defmethod (setf component-property) (new-value (c component) property)
  (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
    (if a
        (setf (cdr a) new-value)
        (setf (slot-value c 'properties)
              (acons property new-value (slot-value c 'properties))))))

(defclass system (module)
  ((description :accessor system-description :initarg :description)
   (long-description
    :accessor system-long-description :initarg :long-description)
   (author :accessor system-author :initarg :author)
   (maintainer :accessor system-maintainer :initarg :maintainer)
   (licence :accessor system-licence :initarg :licence
            :accessor system-license :initarg :license)))

;;; version-satisfies

;;; with apologies to christophe rhodes ...
(defun split (string &optional max (ws '(#\Space #\Tab)))
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))

(defgeneric version-satisfies (component version))

(defmethod version-satisfies ((c component) version)
  (unless (and version (slot-boundp c 'version))
    (return-from version-satisfies t))
  (let ((x (mapcar #'parse-integer
                   (split (component-version c) nil '(#\.))))
        (y (mapcar #'parse-integer
                   (split version nil '(#\.)))))
    (labels ((bigger (x y)
               (cond ((not y) t)
                     ((not x) nil)
                     ((> (car x) (car y)) t)
                     ((= (car x) (car y))
                      (bigger (cdr x) (cdr y))))))
      (and (= (car x) (car y))
           (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding systems

(defvar *defined-systems* (make-hash-table :test 'equal))
(defun coerce-name (name)
   (typecase name
     (component (component-name name))
     (symbol (string-downcase (symbol-name name)))
     (string name)
     (t (sysdef-error "~@<invalid component designator ~A~@:>" name))))

;;; for the sake of keeping things reasonably neat, we adopt a
;;; convention that functions in this list are prefixed SYSDEF-

(defvar *system-definition-search-functions*
  '(sysdef-central-registry-search))

(defun system-definition-pathname (system)
  (some (lambda (x) (funcall x system))
        *system-definition-search-functions*))

(defvar *central-registry*
  '(*default-pathname-defaults*
    #+nil "/home/dan/src/sourceforge/cclan/asdf/systems/"
    #+nil "telent:asdf;systems;"))

(defun sysdef-central-registry-search (system)
  (let ((name (coerce-name system)))
    (block nil
      (dolist (dir *central-registry*)
        (let* ((defaults (eval dir))
               (file (and defaults
                          (make-pathname
                           :defaults defaults :version :newest
                           :name name :type "asd" :case :local))))
          (if (and file (probe-file file))
              (return file)))))))

(defun make-temporary-package ()
  (flet ((try (counter)
           (ignore-errors
                   (make-package (format nil "ASDF~D" counter)
                                 :use '(:cl :asdf)))))
    (do* ((counter 0 (+ counter 1))
          (package (try counter) (try counter)))
         (package package))))

(defun find-system (name &optional (error-p t))
  (let* ((name (coerce-name name))
         (in-memory (gethash name *defined-systems*))
         (on-disk (system-definition-pathname name)))
    (when (and on-disk
               (or (not in-memory)
                   (< (car in-memory) (file-write-date on-disk))))
      (let ((package (make-temporary-package)))
        (unwind-protect
             (let ((*package* package))
               (format
                *verbose-out*
                "~&~@<; ~@;loading system definition from ~A into ~A~@:>~%"
                ;; FIXME: This wants to be (ENOUGH-NAMESTRING
                ;; ON-DISK), but CMUCL barfs on that.
                on-disk
                *package*)
               (load on-disk))
          (delete-package package))))
    (let ((in-memory (gethash name *defined-systems*)))
      (if in-memory
          (progn (if on-disk (setf (car in-memory) (file-write-date on-disk)))
                 (cdr in-memory))
          (if error-p (error 'missing-component :requires name))))))

(defun register-system (name system)
  (format *verbose-out* "~&~@<; ~@;registering ~A as ~A~@:>~%" system name)
  (setf (gethash (coerce-name  name) *defined-systems*)
        (cons (get-universal-time) system)))

(defun system-registered-p (name)
  (gethash (coerce-name name) *defined-systems*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding components

(defgeneric find-component (module name &optional version)
  (:documentation "Finds the component with name NAME present in the
MODULE module; if MODULE is nil, then the component is assumed to be a
system."))

(defmethod find-component ((module module) name &optional version)
  (if (slot-boundp module 'components)
      (let ((m (find name (module-components module)
                     :test #'equal :key #'component-name)))
        (if (and m (version-satisfies m version)) m))))


;;; a component with no parent is a system
(defmethod find-component ((module (eql nil)) name &optional version)
  (let ((m (find-system name nil)))
    (if (and m (version-satisfies m version)) m)))

;;; component subclasses

(defclass source-file (component) ())

(defclass cl-source-file (source-file) ())
(defclass c-source-file (source-file) ())
(defclass java-source-file (source-file) ())
(defclass static-file (source-file) ())
(defclass doc-file (static-file) ())
(defclass html-file (doc-file) ())

(defgeneric source-file-type (component system))
(defmethod source-file-type ((c cl-source-file) (s module)) "lisp")
(defmethod source-file-type ((c c-source-file) (s module)) "c")
(defmethod source-file-type ((c java-source-file) (s module)) "java")
(defmethod source-file-type ((c html-file) (s module)) "html")
(defmethod source-file-type ((c static-file) (s module)) nil)

(defmethod component-relative-pathname ((component source-file))
  (let ((relative-pathname (slot-value component 'relative-pathname)))
    (if relative-pathname
        (merge-pathnames
         relative-pathname
         (make-pathname
          :type (source-file-type component (component-system component))))
        (let* ((*default-pathname-defaults*
                (component-parent-pathname component))
               (name-type
                (make-pathname
                 :name (component-name component)
                 :type (source-file-type component
                                         (component-system component)))))
          name-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operations

;;; one of these is instantiated whenever (operate ) is called

(defclass operation ()
  ((forced :initform nil :initarg :force :accessor operation-forced)
   (original-initargs :initform nil :initarg :original-initargs
                      :accessor operation-original-initargs)
   (visited-nodes :initform nil :accessor operation-visited-nodes)
   (visiting-nodes :initform nil :accessor operation-visiting-nodes)
   (parent :initform nil :initarg :parent :accessor operation-parent)))

(defmethod print-object ((o operation) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (ignore-errors
      (prin1 (operation-original-initargs o) stream))))

(defmethod shared-initialize :after ((operation operation) slot-names
                                     &key force
                                     &allow-other-keys)
  (declare (ignore slot-names force))
  ;; empty method to disable initarg validity checking
  )

(defgeneric perform (operation component))
(defgeneric operation-done-p (operation component))
(defgeneric explain (operation component))
(defgeneric output-files (operation component))
(defgeneric input-files (operation component))

(defun node-for (o c)
  (cons (class-name (class-of o)) c))

(defgeneric operation-ancestor (operation)
  (:documentation   "Recursively chase the operation's parent pointer until we get to the head of the tree"))

(defmethod operation-ancestor ((operation operation))
  (aif (operation-parent operation)
       (operation-ancestor it)
       operation))


(defun make-sub-operation (c o dep-c dep-o)
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


(defgeneric visit-component (operation component data))

(defmethod visit-component ((o operation) (c component) data)
  (unless (component-visited-p o c)
    (push (cons (node-for o c) data)
          (operation-visited-nodes (operation-ancestor o)))))

(defgeneric component-visited-p (operation component))

(defmethod component-visited-p ((o operation) (c component))
  (assoc (node-for o c)
         (operation-visited-nodes (operation-ancestor o))
         :test 'equal))

(defgeneric (setf visiting-component) (new-value operation component))

(defmethod (setf visiting-component) (new-value operation component)
  ;; MCL complains about unused lexical variables
  (declare (ignorable new-value operation component)))

(defmethod (setf visiting-component) (new-value (o operation) (c component))
  (let ((node (node-for o c))
        (a (operation-ancestor o)))
    (if new-value
        (pushnew node (operation-visiting-nodes a) :test 'equal)
        (setf (operation-visiting-nodes a)
              (remove node  (operation-visiting-nodes a) :test 'equal)))))

(defgeneric component-visiting-p (operation component))

(defmethod component-visiting-p ((o operation) (c component))
  (let ((node (cons o c)))
    (member node (operation-visiting-nodes (operation-ancestor o))
            :test 'equal)))

(defgeneric component-depends-on (operation component))

(defmethod component-depends-on ((o operation) (c component))
  (cdr (assoc (class-name (class-of o))
              (slot-value c 'in-order-to))))

(defgeneric component-self-dependencies (operation component))

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

(defmethod input-files ((operation operation) (c module)) nil)

(defmethod operation-done-p ((o operation) (c component))
  (flet ((fwd-or-return-t (file)
           ;; if FILE-WRITE-DATE returns NIL, it's possible that the
           ;; user or some other agent has deleted an input file.  If
           ;; that's the case, well, that's not good, but as long as
           ;; the operation is otherwise considered to be done we
           ;; could continue and survive.
           (let ((date (file-write-date file)))
             (cond
               (date)
               (t
                (warn "~@<Missing FILE-WRITE-DATE for ~S: treating ~
                       operation ~S on component ~S as done.~@:>"
                      file o c)
                (return-from operation-done-p t))))))
    (let ((out-files (output-files o c))
          (in-files (input-files o c)))
      (cond ((and (not in-files) (not out-files))
             ;; arbitrary decision: an operation that uses nothing to
             ;; produce nothing probably isn't doing much
             t)
            ((not out-files)
             (let ((op-done
                    (gethash (type-of o)
                             (component-operation-times c))))
               (and op-done
                    (>= op-done
                        (apply #'max
                               (mapcar #'fwd-or-return-t in-files))))))
            ((not in-files) nil)
            (t
             (and
              (every #'probe-file out-files)
              (> (apply #'min (mapcar #'file-write-date out-files))
                 (apply #'max (mapcar #'fwd-or-return-t in-files)))))))))

;;; So you look at this code and think "why isn't it a bunch of
;;; methods".  And the answer is, because standard method combination
;;; runs :before methods most->least-specific, which is back to front
;;; for our purposes.  And CLISP doesn't have non-standard method
;;; combinations, so let's keep it simple and aspire to portability

(defgeneric traverse (operation component))
(defmethod traverse ((operation operation) (c component))
  (let ((forced nil))
    (labels ((do-one-dep (required-op required-c required-v)
               (let* ((dep-c (or (find-component
                                  (component-parent c)
                                  ;; XXX tacky.  really we should build the
                                  ;; in-order-to slot with canonicalized
                                  ;; names instead of coercing this late
                                  (coerce-name required-c) required-v)
                                 (error 'missing-dependency :required-by c
                                        :version required-v
                                        :requires required-c)))
                      (op (make-sub-operation c operation dep-c required-op)))
                 (traverse op dep-c)))
             (do-dep (op dep)
               (cond ((eq op 'feature)
                      (or (member (car dep) *features*)
                          (error 'missing-dependency :required-by c
                                 :requires (car dep) :version nil)))
                     (t
                      (dolist (d dep)
                        (cond ((consp d)
                               (assert (string-equal
                                        (symbol-name (first d))
                                        "VERSION"))
                               (appendf forced
                                        (do-one-dep op (second d) (third d))))
                              (t
                               (appendf forced (do-one-dep op d nil)))))))))
      (aif (component-visited-p operation c)
           (return-from traverse
             (if (cdr it) (list (cons 'pruned-op c)) nil)))
      ;; dependencies
      (if (component-visiting-p operation c)
          (error 'circular-dependency :components (list c)))
      (setf (visiting-component operation c) t)
      (loop for (required-op . deps) in (component-depends-on operation c)
            do (do-dep required-op deps))
      ;; constituent bits
      (let ((module-ops
             (when (typep c 'module)
               (let ((at-least-one nil)
                     (forced nil)
                     (error nil))
                 (loop for kid in (module-components c)
                       do (handler-case
                              (appendf forced (traverse operation kid ))
                            (missing-dependency (condition)
                              (if (eq (module-if-component-dep-fails c) :fail)
                                  (error condition))
                              (setf error condition))
                            (:no-error (c)
                              (declare (ignore c))
                              (setf at-least-one t))))
                 (when (and (eq (module-if-component-dep-fails c) :try-next)
                            (not at-least-one))
                   (error error))
                 forced))))
        ;; now the thing itself
        (when (or forced module-ops
                  (not (operation-done-p operation c))
                  (let ((f (operation-forced (operation-ancestor operation))))
                    (and f (or (not (consp f))
                               (member (component-name
                                        (operation-ancestor operation))
                                       (mapcar #'coerce-name f)
                                       :test #'string=)))))
          (let ((do-first (cdr (assoc (class-name (class-of operation))
                                      (slot-value c 'do-first)))))
            (loop for (required-op . deps) in do-first
                  do (do-dep required-op deps)))
          (setf forced (append (delete 'pruned-op forced :key #'car)
                               (delete 'pruned-op module-ops :key #'car)
                               (list (cons operation c))))))
      (setf (visiting-component operation c) nil)
      (visit-component operation c (and forced t))
      forced)))


(defmethod perform ((operation operation) (c source-file))
  (sysdef-error
   "~@<required method PERFORM not implemented ~
    for operation ~A, component ~A~@:>"
   (class-of operation) (class-of c)))

(defmethod perform ((operation operation) (c module))
  nil)

(defmethod explain ((operation operation) (component component))
  (format *verbose-out* "~&;;; ~A on ~A~%" operation component))

;;; compile-op

(defclass compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (on-warnings :initarg :on-warnings :accessor operation-on-warnings
                :initform *compile-file-warnings-behaviour*)
   (on-failure :initarg :on-failure :accessor operation-on-failure
               :initform *compile-file-failure-behaviour*)))

(defmethod perform :before ((operation compile-op) (c source-file))
  (map nil #'ensure-directories-exist (output-files operation c)))

(defmethod perform :after ((operation operation) (c component))
  (setf (gethash (type-of operation) (component-operation-times c))
        (get-universal-time))
  (load-preferences c operation))

;;; perform is required to check output-files to find out where to put
;;; its answers, in case it has been overridden for site policy
(defmethod perform ((operation compile-op) (c cl-source-file))
  #-:broken-fasl-loader
  (let ((source-file (component-pathname c))
        (output-file (car (output-files operation c))))
    (multiple-value-bind (output warnings-p failure-p)
                         (compile-file source-file
                                       :output-file output-file)
      ;(declare (ignore output))
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
  #-:broken-fasl-loader (list (compile-file-pathname (component-pathname c)))
  #+:broken-fasl-loader (list (component-pathname c)))

(defmethod perform ((operation compile-op) (c static-file))
  nil)

(defmethod output-files ((operation compile-op) (c static-file))
  nil)

;;; load-op

(defclass basic-load-op (operation) ())

(defclass load-op (basic-load-op) ())

(defmethod perform ((o load-op) (c cl-source-file))
  (mapcar #'load (input-files o c)))

(defmethod perform ((operation load-op) (c static-file))
  nil)
(defmethod operation-done-p ((operation load-op) (c static-file))
  t)

(defmethod output-files ((o operation) (c component))
  nil)

(defmethod component-depends-on ((operation load-op) (c component))
  (cons (list 'compile-op (component-name c))
        (call-next-method)))

;;; load-source-op

(defclass load-source-op (basic-load-op) ())

(defmethod perform ((o load-source-op) (c cl-source-file))
  (let ((source (component-pathname c)))
    (setf (component-property c 'last-loaded-as-source)
          (and (load source)
               (get-universal-time)))))

(defmethod perform ((operation load-source-op) (c static-file))
  nil)

(defmethod output-files ((operation load-source-op) (c component))
  nil)

;;; FIXME: we simply copy load-op's dependencies.  this is Just Not Right.
(defmethod component-depends-on ((o load-source-op) (c component))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (slot-value c 'in-order-to)))))
    (mapcar (lambda (dep)
              (if (eq (car dep) 'load-op)
                  (cons 'load-source-op (cdr dep))
                  dep))
            what-would-load-op-do)))

(defmethod operation-done-p ((o load-source-op) (c source-file))
  (if (or (not (component-property c 'last-loaded-as-source))
          (> (file-write-date (component-pathname c))
             (component-property c 'last-loaded-as-source)))
      nil t))

(defclass test-op (operation) ())

(defmethod perform ((operation test-op) (c component))
  nil)

(defgeneric load-preferences (system operation)
  (:documentation "Called to load system preferences after <perform operation system>. Typical uses are to set parameters that don't exist until after the system has been loaded."))

(defgeneric preference-file-for-system/operation (system operation)
  (:documentation "Returns the pathname of the preference file for this system. Called by 'load-preferences to determine what file to load."))

(defmethod load-preferences ((s t) (operation t))
  ;; do nothing
  (values))

(defmethod load-preferences ((s system) (operation basic-load-op))
  (let* ((*package* (find-package :common-lisp))
         (file (probe-file (preference-file-for-system/operation s operation))))
    (when file
      (when *verbose-out*
        (format *verbose-out*
                "~&~@<; ~@;loading preferences for ~A/~(~A~) from ~A~@:>~%"
                (component-name s)
                (type-of operation) file))
      (load file))))

(defmethod preference-file-for-system/operation ((system t) (operation t))
  ;; cope with anything other than systems
  (preference-file-for-system/operation (find-system system t) operation))

(defmethod preference-file-for-system/operation ((s system) (operation t))
  (merge-pathnames
   (make-pathname :name (component-name s)
                  :type "lisp"
                  :directory '(:relative ".asdf"))
   (truename (user-homedir-pathname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoking operations

(defun operate (operation-class system &rest args &key (verbose t) version
                                &allow-other-keys)
  (let* ((op (apply #'make-instance operation-class
                    :original-initargs args
                    args))
         (*verbose-out* (if verbose *trace-output* (make-broadcast-stream)))
         (system (if (typep system 'component) system (find-system system))))
    (unless (version-satisfies system version)
      (error 'missing-component :requires system :version version))
    (let ((steps (traverse op system)))
      (with-compilation-unit ()
        (loop for (op . component) in steps do
             (loop
                (restart-case
                    (progn (perform op component)
                           (return))
                  (retry ()
                    :report
                    (lambda (s)
                      (format s "~@<Retry performing ~S on ~S.~@:>"
                              op component)))
                  (accept ()
                    :report
                    (lambda (s)
                      (format s
                              "~@<Continue, treating ~S on ~S as ~
                               having been successful.~@:>"
                              op component))
                    (setf (gethash (type-of op)
                                   (component-operation-times component))
                          (get-universal-time))
                    (return)))))))))

(defun oos (&rest args)
  "Alias of OPERATE function"
  (apply #'operate args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax

(defun remove-keyword (key arglist)
  (labels ((aux (key arglist)
             (cond ((null arglist) nil)
                   ((eq key (car arglist)) (cddr arglist))
                   (t (cons (car arglist) (cons (cadr arglist)
                                                (remove-keyword
                                                 key (cddr arglist))))))))
    (aux key arglist)))

(defmacro defsystem (name &body options)
  (destructuring-bind (&key pathname (class 'system) &allow-other-keys) options
    (let ((component-options (remove-keyword :class options)))
      `(progn
        ;; system must be registered before we parse the body, otherwise
        ;; we recur when trying to find an existing system of the same name
        ;; to reuse options (e.g. pathname) from
        (let ((s (system-registered-p ',name)))
          (cond ((and s (eq (type-of (cdr s)) ',class))
                 (setf (car s) (get-universal-time)))
                (s
                 #+clisp
                 (sysdef-error "Cannot redefine the existing system ~A with a different class" s)
                 #-clisp
                 (change-class (cdr s) ',class))
                (t
                 (register-system (quote ,name)
                                  (make-instance ',class :name ',name)))))
        (parse-component-form nil (apply
                                   #'list
                                   :module (coerce-name ',name)
                                   :pathname
                                   (or ,pathname
                                       (pathname-sans-name+type
                                        (resolve-symlinks  *load-truename*))
                                       *default-pathname-defaults*)
                                   ',component-options))))))


(defun class-for-type (parent type)
  (let* ((extra-symbols (list (find-symbol (symbol-name type) *package*)
                              (find-symbol (symbol-name type)
                                           #.(package-name *package*))))
         (class (dolist (symbol (if (keywordp type)
                                    extra-symbols
                                    (cons type extra-symbols)))
                  (when (and symbol
                             (find-class symbol nil)
                             (subtypep symbol 'component))
                    (return (find-class symbol))))))
    (or class
        (and (eq type :file)
             (or (module-default-component-class parent)
                 (find-class 'cl-source-file)))
        (sysdef-error "~@<don't recognize component type ~A~@:>" type))))

(defun maybe-add-tree (tree op1 op2 c)
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

(defun union-of-dependencies (&rest deps)
  (let ((new-tree nil))
    (dolist (dep deps)
      (dolist (op-tree dep)
        (dolist (op  (cdr op-tree))
          (dolist (c (cdr op))
            (setf new-tree
                  (maybe-add-tree new-tree (car op-tree) (car op) c))))))
    new-tree))


(defun remove-keys (key-names args)
  (loop for ( name val ) on args by #'cddr
        unless (member (symbol-name name) key-names
                       :key #'symbol-name :test 'equal)
        append (list name val)))

(defvar *serial-depends-on*)

(defun parse-component-form (parent options)
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
        (setf depends-on (append depends-on (remove-if (complement #'find-system) weakly-depends-on))))
      (when (boundp '*serial-depends-on*)
        (setf depends-on
              (concatenate 'list *serial-depends-on* depends-on)))
      (apply #'reinitialize-instance
             ret
             :name (coerce-name name)
             :pathname pathname
             :parent parent
             other-args)
      (when (typep ret 'module)
        (setf (module-default-component-class ret)
              (or default-component-class
                  (and (typep parent 'module)
                       (module-default-component-class parent))))
        (let ((*serial-depends-on* nil))
          (setf (module-components ret)
                (loop for c-form in components
                      for c = (parse-component-form ret c-form)
                      collect c
                      if serial
                      do (push (component-name c) *serial-depends-on*))))

        ;; check for duplicate names
        (let ((name-hash (make-hash-table :test #'equal)))
          (loop for c in (module-components ret)
                do
                (if (gethash (component-name c)
                             name-hash)
                    (error 'duplicate-names
                           :name (component-name c))
                  (setf (gethash (component-name c)
                                 name-hash)
                        t)))))

      (setf (slot-value ret 'in-order-to)
            (union-of-dependencies
             in-order-to
             `((compile-op (compile-op ,@depends-on))
               (load-op (load-op ,@depends-on))))
            (slot-value ret 'do-first) `((compile-op (load-op ,@depends-on))))

      (loop for (n v) in `((perform ,perform) (explain ,explain)
                           (output-files ,output-files)
                           (operation-done-p ,operation-done-p))
            do (map 'nil
                    ;; this is inefficient as most of the stored
                    ;; methods will not be for this particular gf n
                    ;; But this is hardly performance-critical
                    (lambda (m) (remove-method (symbol-function n) m))
                    (component-inline-methods ret))
            when v
            do (destructuring-bind (op qual (o c) &body body) v
                 (pushnew
                  (eval `(defmethod ,n ,qual ((,o ,op) (,c (eql ,ret)))
                          ,@body))
                  (component-inline-methods ret))))
      ret)))

(defun check-component-input (type name weakly-depends-on depends-on components in-order-to)
  "A partial test of the values of a component."
  (when weakly-depends-on (warn "We got one! XXXXX"))
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

(defun sysdef-error-component (msg type name value)
  (sysdef-error (concatenate 'string msg
                             "~&The value specified for ~(~A~) ~A is ~W")
                type name value))

(defun resolve-symlinks (path)
  #-allegro (truename path)
  #+allegro (excl:pathname-resolve-symbolic-links path)
  )

;;; optional extras

;;; run-shell-command functions for other lisp implementations will be
;;; gratefully accepted, if they do the same thing.  If the docstring
;;; is ambiguous, send a bug report

(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format *verbose-out* "; $ ~A~%" command)
    #+sbcl
    (sb-ext:process-exit-code
     (sb-ext:run-program
      #+win32 "sh" #-win32 "/bin/sh"
      (list  "-c" command)
      #+win32 #+win32 :search t
      :input nil :output *verbose-out*))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))

    #+allegro
    (excl:run-shell-command command :input nil :output *verbose-out*)

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream *verbose-out*)

    #+clisp                             ;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output *verbose-out*
                                 :wait t)))
    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (si:system command)
    #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")
    ))


(defgeneric hyperdocumentation (package name doc-type))
(defmethod hyperdocumentation ((package symbol) name doc-type)
  (hyperdocumentation (find-package package) name doc-type))

(defun hyperdoc (name doc-type)
  (hyperdocumentation (symbol-package name) name doc-type))


(pushnew :asdf *features*)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (sb-ext:posix-getenv "SBCL_BUILDING_CONTRIB")
    (pushnew :sbcl-hooks-require *features*)))

#+(and sbcl sbcl-hooks-require)
(progn
  (defun module-provide-asdf (name)
    (handler-bind ((style-warning #'muffle-warning))
      (let* ((*verbose-out* (make-broadcast-stream))
             (system (asdf:find-system name nil)))
        (when system
          (asdf:operate 'asdf:load-op name)
          t))))

  (defun contrib-sysdef-search (system)
    (let ((home (sb-ext:posix-getenv "SBCL_HOME")))
      (when home
        (let* ((name (coerce-name system))
               (home (truename home))
               (contrib (merge-pathnames
                         (make-pathname :directory `(:relative ,name)
                                        :name name
                                        :type "asd"
                                        :case :local
                                        :version :newest)
                         home)))
          (probe-file contrib)))))

  (pushnew
   '(let ((home (sb-ext:posix-getenv "SBCL_HOME")))
      (when home
        (merge-pathnames "site-systems/" (truename home))))
   *central-registry*)

  (pushnew
   '(merge-pathnames ".sbcl/systems/"
     (user-homedir-pathname))
   *central-registry*)

  (pushnew 'module-provide-asdf sb-ext:*module-provider-functions*)
  (pushnew 'contrib-sysdef-search *system-definition-search-functions*))

(provide 'asdf)

