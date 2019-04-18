;;;; the DEFPACKAGE macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; FIXME: figure out why a full call to FORMAT this early in warm load
;;; says that CLASS is not a known type. (Obviously it needs to parse
;;; a type spec, but then why it is only a style-warning and not an error?)
;;; Weirder still, why does it depend on the target architecture?

(defmacro defpackage (package &rest options)
  #.(locally (declare (notinline format))
     (format nil
  "Defines a new package called PACKAGE. Each of OPTIONS should be one of the
   following: ~{~&~4T~A~}
   All options except ~{~A, ~}and :DOCUMENTATION can be used multiple
   times."
  '((:use "{package-name}*")
    (:export "{symbol-name}*")
    (:import-from "<package-name> {symbol-name}*")
    (:shadow "{symbol-name}*")
    (:shadowing-import-from "<package-name> {symbol-name}*")
    (:local-nicknames "{(local-nickname actual-package-name)}*")
    (:lock "boolean")
    (:implement "{package-name}*")
    (:documentation "doc-string")
    (:intern "{symbol-name}*")
    (:size "<integer>")
    (:nicknames "{package-name}*"))
  '(:size :lock)))
  (let ((nicknames nil)
        (local-nicknames nil)
        (size nil)
        (shadows nil)
        (shadowing-imports nil)
        (use nil)
        (use-p nil)
        (imports nil)
        (interns nil)
        (exports nil)
        (package (stringify-string-designator package))
        (implement nil)
        (implement-p nil)
        (lock nil)
        (doc nil)
        (optname nil)
        (optval nil)
        (seen nil))
    (dolist (option options)
      (unless (consp option)
        (%program-error "bogus DEFPACKAGE option: ~S" option))
      (setq optname (car option) optval (cdr option))
      (case optname
        ((:documentation :size :lock)
         (when (memq optname seen)
           (%program-error "can't specify ~S more than once." optname))
         (unless (typep optval '(cons t null))
           (%program-error "~S expects a single argument. Got ~S"
                          (car option) (cdr option)))
         (push optname seen)
         (setq optval (car optval))))
      (case optname
        (:nicknames
         (setf nicknames
               (append nicknames (stringify-string-designators optval))))
        (:local-nicknames
         (setf local-nicknames
               (append local-nicknames
                       (mapcar (lambda (spec)
                                 (destructuring-bind (nick name) spec
                                   (cons (stringify-package-designator nick)
                                         (stringify-package-designator name))))
                               optval))))
        (:size
         (if (typep optval 'unsigned-byte)
             (setf size optval)
             (%program-error ":SIZE is not a positive integer: ~S" option)))
        (:shadow
         (setf shadows (append shadows (stringify-string-designators optval))))
        (:shadowing-import-from
         (let ((package-name (stringify-package-designator (car optval)))
               (names (stringify-string-designators (cdr optval))))
           (let ((assoc (assoc package-name shadowing-imports :test #'string=)))
             (if assoc
                 (setf (cdr assoc) (append (cdr assoc) names))
                 (setf shadowing-imports
                       (acons package-name names shadowing-imports))))))
        (:use
         (setf use (append use (stringify-package-designators optval))
               use-p t))
        (:import-from
         (let ((package-name (stringify-package-designator (car optval)))
               (names (stringify-string-designators (cdr optval))))
           (let ((assoc (assoc package-name imports :test #'string=)))
             (if assoc
                 (setf (cdr assoc) (append (cdr assoc) names))
                 (setf imports (acons package-name names imports))))))
        (:intern
         (setf interns (append interns (stringify-string-designators optval))))
        (:export
         (setf exports (append exports (stringify-string-designators optval))))
        (:implement
         (setf implement (append implement (stringify-package-designators optval))
               implement-p t))
        (:lock
         (setf lock (coerce optval 'boolean)))
        (:documentation
         (setf doc (possibly-base-stringize optval)))
        (t
         (%program-error "bogus DEFPACKAGE option: ~S" option))))
    (check-disjoint `(:intern ,@interns) `(:export  ,@exports))
    (check-disjoint `(:intern ,@interns)
                    `(:import-from
                      ,@(apply #'append (mapcar #'rest imports)))
                    `(:shadow ,@shadows)
                    `(:shadowing-import-from
                      ,@(apply #'append (mapcar #'rest shadowing-imports))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%defpackage ,package ',nicknames ',size
                    ',shadows ',shadowing-imports ',(if use-p use :default)
                    ',imports ',interns ',exports
                    ;; FIXME: the default singleton list seems unnecessary.
                    ;; PACKAGE-LOCK-VIOLATION-P considers every package to implement
                    ;; itself. Additionally there's an obvious inconsistency:
                    ;; * (package-implements-list (defpackage "A")) => (#<PACKAGE "A">)
                    ;; * (package-implements-list (make-package "B")) => NIL
                    ',(if implement-p implement (list package))
                    ',local-nicknames
                    ',lock (sb-c:source-location)
                    ,@(and doc
                           `(,doc))))))

(defun check-disjoint (&rest args)
  ;; An arg is (:key . set)
  (do ((list args (cdr list)))
      ((endp list))
    (loop
      with x = (car list)
      for y in (rest list)
      for z = (remove-duplicates (intersection (cdr x)(cdr y) :test #'string=))
      when z do (%program-error "Parameters ~S and ~S must be disjoint ~
                                 but have common elements ~%   ~S"
                                (car x) (car y) z))))

(flet ((designator-to-string (kind designator)
         (cond ((and (eq kind 'package) (packagep designator))
                (package-name designator)) ; already simple and basic if possible
               (t
                (possibly-base-stringize
                 (cond ((stringp designator) designator)
                       ((symbolp designator) (symbol-name designator))
                       ((characterp designator) (string designator))
                       (t (error 'simple-type-error
                                 :datum designator
                                 :expected-type
                                 (if (eq kind 'package) 'package-designator 'string-designator)
                                 :format-control "~S does not designate a ~(~A~)"
                                 :format-arguments (list designator kind)))))))))
  (defun stringify-string-designator (string-designator)
    (designator-to-string 'string string-designator))
  (defun stringify-package-designator (package-designator)
    (designator-to-string 'package package-designator)))

(defun stringify-string-designators (string-designators)
  (mapcar #'stringify-string-designator string-designators))

(defun stringify-package-designators (package-designators)
  (mapcar #'stringify-package-designator package-designators))

(defun import-list-symbols (import-list)
  (let ((symbols nil))
    (dolist (import import-list symbols)
      (destructuring-bind (package-name &rest symbol-names)
          import
        (let ((package (find-undeleted-package-or-lose package-name)))
          (mapcar (lambda (name)
                    (push (find-or-make-symbol name package) symbols))
                  symbol-names))))))

(defun use-list-packages (package package-designators)
  (cond ((listp package-designators)
         (mapcar #'find-undeleted-package-or-lose package-designators))
        (package
         ;; :default for an existing package means preserve the
         ;; existing use list
         (package-use-list package))
        (t
         ;; :default for a new package is the *!default-package-use-list*
         '#.*!default-package-use-list*)))

(defun update-package (package nicknames source-location
                       shadows shadowing-imports
                       use
                       imports interns
                       exports implement local-nicknames
                       lock doc-string)
  (%enter-new-nicknames package nicknames)
  ;; 1. :shadow and :shadowing-import-from
  ;;
  ;; shadows is a list of strings, shadowing-imports is a list of symbols.
  (shadow shadows package)
  (shadowing-import shadowing-imports package)
  ;; 2. :use
  ;;
  ;; use is a list of package objects.
  (use-package use package)
  ;; 3. :import-from and :intern
  ;;
  ;; imports is a list of symbols. interns is a list of strings.
  (import imports package)
  (dolist (intern interns)
    (intern intern package))
  ;; 4. :export
  ;;
  ;; exports is a list of strings
  (export (mapcar (lambda (symbol-name) (intern symbol-name package))
                  exports)
          package)
  ;; 5. :local-nicknames
  ;; FIXME: See bug at PACKAGE-LOCALLY-NICKNAMED-BY-LIST
  (setf (package-%local-nicknames package) nil) ; throw out the old ones.
  (loop :for (nickname . nickname-package) :in local-nicknames :do
     (%add-package-local-nickname nickname nickname-package package))
  ;; Everything was created: update metadata
  (when source-location
    (setf (package-source-location package) source-location))
  (setf (package-doc-string package) doc-string)
  ;; Handle packages this is an implementation package of
  (dolist (p implement)
      (add-implementation-package package p))
  ;; Handle lock
  (setf (package-lock package) lock)
  ;; Flush cached FIND-PACKAGE values
  (atomic-incf *package-names-cookie*)
  package)

(declaim (type list *on-package-variance*))
(defvar *on-package-variance* '(:warn t)
  "Specifies behavior when redefining a package using DEFPACKAGE and the
definition is in variance with the current state of the package.

The value should be of the form:

  (:WARN [T | packages-names] :ERROR [T | package-names])

specifying which packages get which behaviour -- with T signifying the default unless
otherwise specified. If default is not specified, :WARN is used.

:WARN keeps as much state as possible and causes SBCL to signal a full warning.

:ERROR causes SBCL to signal an error when the variant DEFPACKAGE form is executed,
with restarts provided for user to specify what action should be taken.

Example:

  (setf *on-package-variance* '(:warn (:swank :swank-backend) :error t))

specifies to signal a warning if SWANK package is in variance, and an error otherwise.")

(defun note-package-variance (&rest args &key package &allow-other-keys)
  (let ((pname (package-name package)))
    (destructuring-bind (&key warn error) *on-package-variance*
      (let ((what (cond ((and (listp error) (member pname error :test #'string=))
                         :error)
                        ((and (listp warn) (member pname warn :test #'string=))
                         :warn)
                        ((eq t error)
                         :error)
                        (t
                         :warn))))
        (ecase what
          (:error
           (apply #'error 'package-at-variance-error args))
          (:warn
           (apply #'warn 'package-at-variance args)))))))

(defun update-package-with-variance (package name nicknames source-location
                                     shadows shadowing-imports
                                     use
                                     imports interns
                                     exports
                                     implement local-nicknames
                                     lock doc-string)
  (unless (string= (the string (package-name package)) name)
    (error 'simple-package-error
           :package name
           :format-control "~A is a nickname for the package ~A"
           :format-arguments (list name (package-name name))))
  (let ((no-longer-shadowed
          (set-difference (package-%shadowing-symbols package)
                          (append shadows shadowing-imports)
                          :test #'string=)))
    (when no-longer-shadowed
      (restart-case
          (let ((*package* (find-package :keyword)))
            (note-package-variance
             :format-control "~A also shadows the following symbols:~%  ~S"
             :format-arguments (list name no-longer-shadowed)
             :package package))
        (drop-them ()
          :report "Stop shadowing them by uninterning them."
          (dolist (sym no-longer-shadowed)
            (unintern sym package)))
        (keep-them ()
          :report "Keep shadowing them."))))
  (let ((no-longer-used (set-difference (package-use-list package) use)))
    (when no-longer-used
      (restart-case
          (note-package-variance
           :format-control "~A also uses the following packages:~%  ~A"
           :format-arguments (list name (mapcar #'package-name no-longer-used))
           :package package)
        (drop-them ()
          :report "Stop using them."
          (unuse-package no-longer-used package))
        (keep-them ()
          :report "Keep using them."))))
  (let (old-exports)
    (do-external-symbols (s package)
      (push s old-exports))
    (let ((no-longer-exported (set-difference old-exports exports :test #'string=)))
     (when no-longer-exported
       (restart-case
           (note-package-variance
            :format-control "~A also exports the following symbols:~%  ~S"
            :format-arguments (list name no-longer-exported)
            :package package)
         (drop-them ()
           :report "Unexport them."
           (unexport no-longer-exported package))
         (keep-them ()
           :report "Keep exporting them.")))))
  (let ((old-implements
          (set-difference (package-implements-list package)
                          (mapcar #'find-undeleted-package-or-lose implement))))
    (when old-implements
      (restart-case
          (note-package-variance
           :format-control "~A is also an implementation package for:~% ~{~S~^~%  ~}"
           :format-arguments (list name old-implements)
           :package package)
        (drop-them ()
          :report "Stop being an implementation package for them."
          (dolist (p old-implements)
            (remove-implementation-package package p)))
        (keep-them ()
          :report "Keep exporting them."))))
  (update-package package nicknames source-location
                  shadows shadowing-imports
                  use imports interns exports
                  implement local-nicknames
                  lock doc-string))

(defun %defpackage (name nicknames size shadows shadowing-imports
                    use imports interns exports implement local-nicknames
                    lock source-location &optional doc)
  (declare (type simple-string name)
           (type list nicknames shadows shadowing-imports
                 imports interns exports)
           (type (or list (member :default)) use)
           (type (or simple-string null) doc))
  (with-package-graph ()
    (let* ((existing-package (find-package name))
           (use (use-list-packages existing-package use))
           (shadowing-imports (import-list-symbols shadowing-imports))
           (imports (import-list-symbols imports)))
      (if existing-package
          (update-package-with-variance existing-package name
                                        nicknames source-location
                                        shadows shadowing-imports
                                        use imports interns exports
                                        implement local-nicknames
                                        lock doc)
          (let ((package (make-package name
                                       :use nil
                                       :internal-symbols (or size 10)
                                       :external-symbols (length exports))))
            (update-package package
                            nicknames
                            source-location
                            shadows shadowing-imports
                            use imports interns exports
                            implement local-nicknames
                            lock doc))))))

(defun find-or-make-symbol (name package)
  (multiple-value-bind (symbol how) (find-symbol name package)
    (cond (how
           symbol)
          (t
           (with-simple-restart (continue "INTERN it.")
             (error 'simple-package-error
                    :package package
                    :format-control "no symbol named ~S in ~S"
                    :format-arguments (list name (package-name package))))
           (intern name package)))))

;;;; One more package-related transform. I'm unsure why this can't be installed
;;;; sooner, but it can't. It's a symptom of our build weirdness involving
;;;; the lack of a class definition for the class CLASS, but I can't see what
;;;; that's got to do with anything.
(in-package "SB-C")

;;; As for the INTERN transform, you could be screwed if you use any of the
;;; standard package names as a local nickname of a random package.
(deftransform find-package ((name) ((constant-arg string-designator)))
  (multiple-value-bind (form constp) (find-package-xform name)
    ;; standard packages are effectively constant objects, otherwise
    ;; we have to invoke the find function.
    (if constp form `(sb-impl::cached-find-package ,form))))

;;;; package hacking

;;; FIXME: This nickname is a deprecated hack for backwards
;;; compatibility with code which assumed the CMU-CL-style
;;; SB-ALIEN/SB-C-CALL split. That split went away and was deprecated
;;; in 0.7.0, so we should get rid of this nickname after a while.
(let ((package (find-package "SB-ALIEN")))
  (rename-package package package
                  (cons "SB-C-CALL" (package-nicknames package))))

(let ((package (find-package "SB-SEQUENCE")))
  (rename-package package package (list "SEQUENCE")))
