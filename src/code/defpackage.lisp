;;;; the DEFPACKAGE macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; the list of packages to use by default when no :USE argument is
;;; supplied to MAKE-PACKAGE or other package creation forms
;;;
;;; ANSI specifies (1) that MAKE-PACKAGE and DEFPACKAGE use the same
;;; value, and (2) that it (as an implementation-defined value) should
;;; be documented, which we do in the doc string. So for OAOO reasons
;;; we represent this value as a variable only at compile time, and
;;; then use #. readmacro hacks to splice it into the target code as a
;;; constant.
(eval-when (:compile-toplevel)
  (defparameter *default-package-use-list*
    ;; ANSI says this is implementation-defined. So we make it NIL,
    ;; the way God intended. Anyone who actually wants a random value
    ;; is free to :USE (PACKAGE-USE-LIST :CL-USER) anyway.:-|
    nil))

(defmacro defpackage (package &rest options)
  #!+sb-doc
  #.(format nil
  "Defines a new package called PACKAGE. Each of OPTIONS should be one of the
   following: ~{~&~4T~A~}
   All options except ~{~A, ~}and :DOCUMENTATION can be used multiple
   times."
  '((:nicknames "{package-name}*")
    (:size "<integer>")
    (:shadow "{symbol-name}*")
    (:shadowing-import-from "<package-name> {symbol-name}*")
    (:use "{package-name}*")
    (:import-from "<package-name> {symbol-name}*")
    (:intern "{symbol-name}*")
    (:export "{symbol-name}*")
    #!+sb-package-locks (:implement "{package-name}*")
    #!+sb-package-locks (:lock "boolean")
    (:documentation "doc-string"))
  '(:size #!+sb-package-locks :lock))
  (let ((nicknames nil)
        (size nil)
        (shadows nil)
        (shadowing-imports nil)
        (use nil)
        (use-p nil)
        (imports nil)
        (interns nil)
        (exports nil)
        (implement (stringify-package-designators (list package)))
        (implement-p nil)
        (lock nil)
        (doc nil))
    #!-sb-package-locks
    (declare (ignore implement-p))
    (dolist (option options)
      (unless (consp option)
        (error 'simple-program-error
               :format-control "bogus DEFPACKAGE option: ~S"
               :format-arguments (list option)))
      (case (car option)
        (:nicknames
         (setf nicknames (stringify-package-designators (cdr option))))
        (:size
         (cond (size
                (error 'simple-program-error
                       :format-control "can't specify :SIZE twice."))
               ((and (consp (cdr option))
                     (typep (second option) 'unsigned-byte))
                (setf size (second option)))
               (t
                (error
                 'simple-program-error
                 :format-control ":SIZE is not a positive integer: ~S"
                 :format-arguments (list (second option))))))
        (:shadow
         (let ((new (stringify-string-designators (cdr option))))
           (setf shadows (append shadows new))))
        (:shadowing-import-from
         (let ((package-name (stringify-package-designator (second option)))
               (names (stringify-string-designators (cddr option))))
           (let ((assoc (assoc package-name shadowing-imports
                               :test #'string=)))
             (if assoc
                 (setf (cdr assoc) (append (cdr assoc) names))
                 (setf shadowing-imports
                       (acons package-name names shadowing-imports))))))
        (:use
         (setf use (append use (stringify-package-designators (cdr option)) )
               use-p t))
        (:import-from
         (let ((package-name (stringify-package-designator (second option)))
               (names (stringify-string-designators (cddr option))))
           (let ((assoc (assoc package-name imports
                               :test #'string=)))
             (if assoc
                 (setf (cdr assoc) (append (cdr assoc) names))
                 (setf imports (acons package-name names imports))))))
        (:intern
         (let ((new (stringify-string-designators (cdr option))))
           (setf interns (append interns new))))
        (:export
         (let ((new (stringify-string-designators (cdr option))))
           (setf exports (append exports new))))
        #!+sb-package-locks
        (:implement
         (unless implement-p
           (setf implement nil))
         (let ((new (stringify-package-designators (cdr option))))
           (setf implement (append implement new)
                 implement-p t)))
        #!+sb-package-locks
        (:lock
         (when lock
           (error 'simple-program-error
                  :format-control "multiple :LOCK options"))
         (setf lock (coerce (second option) 'boolean)))
        (:documentation
         (when doc
           (error 'simple-program-error
                  :format-control "multiple :DOCUMENTATION options"))
         (setf doc (coerce (second option) 'simple-string)))
        (t
         (error 'simple-program-error
                :format-control "bogus DEFPACKAGE option: ~S"
                :format-arguments (list option)))))
    (check-disjoint `(:intern ,@interns) `(:export  ,@exports))
    (check-disjoint `(:intern ,@interns)
                    `(:import-from
                      ,@(apply #'append (mapcar #'rest imports)))
                    `(:shadow ,@shadows)
                    `(:shadowing-import-from
                      ,@(apply #'append (mapcar #'rest shadowing-imports))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%defpackage ,(stringify-string-designator package) ',nicknames ',size
                    ',shadows ',shadowing-imports ',(if use-p use :default)
                    ',imports ',interns ',exports ',implement ',lock ',doc
                    (sb!c:source-location)))))

(defun check-disjoint (&rest args)
  ;; An arg is (:key . set)
  (do ((list args (cdr list)))
      ((endp list))
    (loop
      with x = (car list)
      for y in (rest list)
      for z = (remove-duplicates (intersection (cdr x)(cdr y) :test #'string=))
      when z do (error 'simple-program-error
                       :format-control "Parameters ~S and ~S must be disjoint ~
                                        but have common elements ~%   ~S"
                       :format-arguments (list (car x)(car y) z)))))

(defun stringify-string-designator (string-designator)
  (typecase string-designator
    (simple-string string-designator)
    (string (coerce string-designator 'simple-string))
    (symbol (symbol-name string-designator))
    (character (string string-designator))
    (t
     (error "~S does not designate a string" string-designator))))

(defun stringify-string-designators (string-designators)
  (mapcar #'stringify-string-designator string-designators))

(defun stringify-package-designator (package-designator)
  (typecase package-designator
    (simple-string package-designator)
    (string (coerce package-designator 'simple-string))
    (symbol (symbol-name package-designator))
    (character (string package-designator))
    (package (package-name package-designator))
    (t
     (error "~S does not designate a package" package-designator))))

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
         ;; :default for a new package is the *default-package-use-list*
         '#.*default-package-use-list*)))

(defun update-package (package nicknames source-location
                       shadows shadowing-imports
                       use
                       imports interns
                       exports
                       implement lock doc-string)
  (declare #!-sb-package-locks
           (ignore implement lock))
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
  ;; Everything was created: update metadata
  (sb!c:with-source-location (source-location)
    (setf (package-source-location package) source-location))
  (setf (package-doc-string package) doc-string)
  #!+sb-package-locks
  (progn
    ;; Handle packages this is an implementation package of
    (dolist (p implement)
      (add-implementation-package package p))
    ;; Handle lock
    (setf (package-lock package) lock))
  package)

(defun update-package-with-variance (package name nicknames source-location
                                     shadows shadowing-imports
                                     use
                                     imports interns
                                     exports
                                     implement lock doc-string)
  (let ((old-exports nil)
        (old-shadows (package-%shadowing-symbols package))
        (old-use (package-use-list package))
        (no-longer-used nil))
    (unless (string= (the string (package-name package)) name)
      (error 'simple-package-error
             :package name
             :format-control "~A is a nickname for the package ~A"
             :format-arguments (list name (package-name name))))
    (do-external-symbols (symbol package)
      (push symbol old-exports))
    (setf old-shadows (set-difference old-shadows (append shadows
                                                          shadowing-imports)
                                      :test #'string=))
    (setf no-longer-used (set-difference old-use use))
    (setf use (set-difference use old-use))
    (setf old-exports (set-difference old-exports exports :test #'string=))
    (when old-shadows
      (warn 'package-at-variance
            :format-control "~A also shadows the following symbols:~%  ~S"
            :format-arguments (list name old-shadows)))
    (when no-longer-used
      (dolist (unused-package no-longer-used)
        (unuse-package unused-package package))
      (warn 'package-at-variance
            :format-control "~A used to use the following packages:~%  ~S"
            :format-arguments (list name no-longer-used)))
    (when old-exports
      (warn 'package-at-variance
            :format-control "~A also exports the following symbols:~%  ~S"
            :format-arguments (list name old-exports)))
    (update-package package nicknames source-location
                    shadows shadowing-imports
                    use imports interns exports
                    implement lock doc-string)))

(defun %defpackage (name nicknames size shadows shadowing-imports
                    use imports interns exports implement lock doc-string
                    source-location)
  (declare (type simple-string name)
           (type list nicknames shadows shadowing-imports
                 imports interns exports)
           (type (or list (member :default)) use)
           (type (or simple-string null) doc-string)
           #!-sb-package-locks
           (ignore implement lock))
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
                                        implement lock doc-string)
          (let ((package (make-package name
                                       :use nil
                                       :internal-symbols (or size 10)
                                       :external-symbols (length exports))))
            (update-package package
                            nicknames source-location
                            shadows shadowing-imports
                            use imports interns exports
                            implement lock doc-string))))))

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
