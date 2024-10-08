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

;;; ANSI specifies that:
;;;  (1) MAKE-PACKAGE and DEFPACKAGE use the same default package-use-list
;;;  (2) that it (as an implementation-defined value) should be documented,
;;;      which we do in the doc string.
;;; For OAOO reasons we give a name to this value and then use #. readmacro
;;; to splice it in as a constant. Anyone who actually wants a random value
;;; is free to :USE (PACKAGE-USE-LIST :CL-USER) or whatever.
(defglobal *!default-package-use-list* nil)

(defmacro sanitize-nicknames (name list)
  `(let ((list ,list))
     (when list
       (remove ,name (remove-duplicates (stringify-string-designators list) :test 'string=)
               :test 'string=))))

(defun make-package (name &key
                          (use '#.*!default-package-use-list*)
                          nicknames
                          (internal-symbols 10)
                          (external-symbols 10))
  #.(format nil
     "Make a new package having the specified NAME, NICKNAMES, and USE
list. :INTERNAL-SYMBOLS and :EXTERNAL-SYMBOLS are estimates for the number of
internal and external symbols which will ultimately be present in the package.
The default value of USE is implementation-dependent, and in this
implementation it is ~S." *!default-package-use-list*)
  (let* ((name (stringify-string-designator name))
         (nicks (sanitize-nicknames name nicknames))
         (package
          ;; a "resolved" package is not in the global name->package mapping yet,
          ;; which is why the FIND-PACKAGE / CERROR below does not signal.
          (or (resolve-deferred-package name)
              (%make-package (make-symbol-table internal-symbols)
                             (make-symbol-table external-symbols))))
         (existing-pkg)
         (namelist (cons name nicks))
         (conflict))
    (setf (symtbl-package (package-external-symbols package)) package)
    (with-package-names (table) ; get exclusive use of name -> package mapping
      ;; If the loop runs to completion, then insert all names,
      ;; which also assigns the %NAME and KEYS slots of the package.
      (dolist (string namelist (package-registry-update package namelist))
        (when (setq existing-pkg (%get-package string table))
          (return (setq conflict string)))))
    (when existing-pkg
      ;; Of the possible ways to continue from the "package already exists" error,
      ;; I've seen implementations offer these:
      ;;  - pick a different name for the new package
      ;;  - return the existing package as-is
      ;; Other ways to proceed might be:
      ;;  - rename the existing package to something different (mostly harmless)
      ;;  - first delete the existing package (potentially nontrivial)
      ;; SBCL had the weirdest of all solutions: alter the existing name->package mapping
      ;; while leaving the old package with a name, but not findable by that name.
      ;; Imho the safe assumption is that the user wants the same package back.
      ;; All sorts of crazy restarts would in theory be possible, such as if one nickname
      ;; that you specified (N1) already finds package P1, and another nickname (N2) finds P2
      ;; and P1 and P2 are distinct. What are we supposed to do? Figure our that you meant
      ;; to return the existing P1, but give it an additional nickname, and delete P2?
      ;; Also bear in mind that for most purposes, a package's multiple "names" are
      ;; equivalent; one is canonical for printing symbols homed in that package.
      ;; Global nicknames are an absurdly unnecessary part of the language.
      (signal-package-cerror name "Return the existing package."
                             "A package named ~S already exists" conflict)
      ;; We don't do the USE in this case.
      ;; It's OK given the lack of guidance in CLHS about how to "continue".
      (return-from make-package existing-pkg))
    (atomic-incf *package-names-cookie*)
    (when (boundp 'sb-c::*compilation*)
      (setf (sb-c::package-environment-changed sb-c::*compilation*) t))
    (use-package use package)
    package))

;;; Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
;;; The spec says that NAME is a package designator (not just a string designator)
;;; which is weird, but potentially meaningful if assigning new global nicknames.
;;; If not called for that purpose, then it's largely pointless, because you can't
;;; rename to any package-designator other than itself without causing a conflict.
;;; A survey of some other implementations suggests that we're in the minority
;;; as to the legality of (RENAME-PACKAGE "A" (FIND-PACKAGE "A") '("A-NICK")).
;;;
;;; ABCL:
;;;   The value #<PACKAGE A> is not of type (OR STRING SYMBOL CHARACTER).
;;; CCL:
;;;   Error: The value #<Package "A"> is not of the expected type (OR STRING SYMBOL CHARACTER).
;;; CMUCL:
;;;   #<The A package, 0/9 internal, 0/9 external> cannot be coerced to a string.
;;; ECL:
;;;   In function STRING, the value of the first argument is #<"A" package>
;;;   which is not of the expected type STRING
;;;
;;; CLISP agrees with us that this usage is permitted. If the new "name" is a
;;; different package, it is merely the same error as if any already-existing name
;;; was given. I see no reason to be more strict than the spec would have it be.
(defun rename-package (package-designator name &optional (nicknames ()))
  "Changes the name and nicknames for a package."
  ;; CLHS says:
  ;; "The consequences are undefined if new-name or any new-nickname
  ;; conflicts with any existing package names."
  ;; Signaling an error is what most implementations do. So shall we now.
  ;; (There is no portable standard way to proceed)
  (let* ((package (find-undeleted-package-or-lose package-designator))
         ;; This potentially allows the "weirdness" alluded to above
         (name (stringify-package-designator name))
         (nicknames (sanitize-nicknames name nicknames))
         (namelist (cons name nicknames))
         (conflict))
    (with-single-package-locked-error ()
      (unless (and (string= name (package-name package))
                   (null (set-difference nicknames (package-%nicknames package)
                                         :test #'string=))
                   (null (set-difference (package-%nicknames package) nicknames
                                         :test #'string=)))
        (assert-package-unlocked
         package "renaming as ~A~@[ with nickname~*~P ~1@*~{~A~^, ~}~]"
         name nicknames (length nicknames))))
    (with-package-names (table)
      ;; get exclusive use of name -> package mapping
      ;; and also prevent concurrent modification to this package's names.
      (dolist (string namelist (package-registry-update package namelist))
        (let ((found (%get-package string table)))
          (cond ((eq found package))
                (found (return (setq conflict string)))))))
    (cond (conflict
           (signal-package-error
            ;; avoid saying "another package ... has name X" so the pedants
            ;; don't complain when X is a "nickname" rather than "name"
            package "Another package is already accessible via name ~S" conflict))
          (t
           (atomic-incf *package-names-cookie*)
           (when (boundp 'sb-c::*compilation*)
             (setf (sb-c::package-environment-changed sb-c::*compilation*) t))
           package))))

(defun delete-package (package-designator)
  "Delete the package designated by PACKAGE-DESIGNATOR from the package
  system data structures."
  (when (and (packagep package-designator)
             (not (package-%name package-designator))) ; already deleted
    (return-from delete-package nil))
  (tagbody :restart
     (let ((package (find-package package-designator)))
       (cond ((not package)
              ;; This continuable error is required by ANSI.
              (signal-package-cerror
               package-designator
               "Ignore."
               "There is no package named ~S." package-designator)
              (return-from delete-package nil))
             (t
              (with-single-package-locked-error
                  (:package package "deleting package ~A" package)
                (let ((use-list (package-used-by-list package)))
                  (when use-list
                    ;; This continuable error is specified by ANSI.
                    (signal-package-cerror
                     package
                     "Remove dependency in other packages."
                     "~@<Package ~S is used by package~P:~2I~_~S~@:>"
                     (package-name package)
                     (length use-list)
                     (mapcar #'package-name use-list))
                    (dolist (p use-list)
                      (unuse-package package p))))
                (dolist (p (package-implements-list package))
                  (remove-implementation-package package p))
                (with-package-graph ()
                  ;; Check for races, restart if necessary.
                  (let ((package2 (find-package package-designator)))
                    (when (or (neq package package2) (package-used-by-list package2))
                      (go :restart)))
                  (dolist (used (package-use-list package))
                    (unuse-package used package))
                  (setf (package-%local-nicknames package) nil)
                  (flet ((nullify-home (symbols)
                           (dovector (x (symtbl-cells symbols))
                             (when (and (symbolp x)
                                        (eq (symbol-package x) package))
                               (%set-symbol-package x nil)))))
                    (nullify-home (package-internal-symbols package))
                    (nullify-home (package-external-symbols package)))
                  (with-package-names ()
                    (package-registry-update package nil)
                    (awhen (package-id package)
                      (setf (aref *id->package* it) nil (package-id package) nil))
                    (setf (package-%name package) nil
                          ;; Setting PACKAGE-%NAME to NIL is required in order to
                          ;; make PACKAGE-NAME return NIL for a deleted package as
                          ;; ANSI requires. Setting the other slots to NIL
                          ;; and blowing away the SYMBOL-TABLEs is just done
                          ;; for tidiness and to help the GC.
                          (package-keys package) #()))
                  (atomic-incf *package-names-cookie*)
                  (when (boundp 'sb-c::*compilation*)
                    (setf (sb-c::package-environment-changed sb-c::*compilation*) t))
                  (setf (package-tables package) #()
                        (package-%shadowing-symbols package) nil
                        (package-internal-symbols package) (make-symbol-table 0)
                        (package-external-symbols package) (make-symbol-table 0)))
                (return-from delete-package t)))))))

;;; Possible FIXME:
;;;   After doing these 2 things:
;;;    (defpackage "P" (:nicknames "PNICK" "PNICK2"))
;;;    (defpackage "P" (:nicknames "NN"))
;;;   how many nicknames has "P" - 1, 2, 3?
;;;   The case for 1 is that DEFPACKAGE says:
;;;      "The arguments to :nicknames set the package's nicknames to the supplied names."
;;       [ECL interprets the redefinition thusly]
;;;   The case for 2 is that it says:
;;;      "If defined-package-name already refers to an existing package,
;;;       the name-to-package mapping for that name is not changed."
;;;      [ABCL interprets the redefinition thusly]
;;;   The case for 3 is that nicknames are cumulative.
;;;      [SBCL interprets the redefinition thusly]
;;; I suspect that any treatment is fine as long as its documented.
;;; Relatedly, ECL and ABCL take a redefinition with 0 nicknames as follows:
;;;  (defpackage "P" (:nicknames))
;;; to imply _no_ _change_ to existing nicknames, while CLISP removes any nicknames.

(defmacro defpackage (package &rest options)
  #.(format nil
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
     '(:size :lock))
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

(flet ((designator-to-string (designator type format-control)
         (possibly-base-stringize
          (typecase designator
            (string designator)
            (symbol (symbol-name designator))
            (character (string designator))
            (t (error 'simple-type-error
                      :datum designator
                      :expected-type type
                      :format-control format-control
                      :format-arguments (list designator)))))))
  (defun stringify-string-designator (string-designator)
    (designator-to-string string-designator 'string-designator
                          "~S does not designate a string"))
  (defun stringify-package-designator (package-designator)
    (if (packagep package-designator)
        (package-name package-designator) ; already simple, and base-string when possible
        (designator-to-string package-designator 'package-designator
                              "~S does not designate a package"))))

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
  (rename-package package (package-name package) nicknames)
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

;;;; APROPOS and APROPOS-LIST

(defun briefly-describe-symbol (symbol)
  (fresh-line)
  (prin1 symbol)
  (when (boundp symbol)
    (let ((value (symbol-value symbol)))
      (if (typep value '(or fixnum symbol hash-table))
          (format t " = ~S" value)
          (format t " (bound, ~S)" (type-of value)))))
  (when (fboundp symbol)
    (write-string " (fbound)")))

(flet ((add-to-bag-if-found (table string length hash result)
         (with-symbol ((symbol) table string length hash)
           ;; HASH-TABLE degenerates to a list when used by FIND-ALL-SYMBOLS
           ;; since homographs have the same SXHASH, so handle either
           ;; a hash-table or a cons containing a list.
           (if (hash-table-p result)
               (setf (gethash symbol result) t)
               (pushnew symbol (car result))))))

(defun find-all-symbols (string-designator)
  "Return a list of all symbols in the system having the specified name."
  (let* ((string (truly-the simple-string
                            (stringify-string-designator string-designator)))
         (length (length string))
         (hash (calc-symbol-name-hash string length))
         (result (list nil)))
    (do-packages (p) ; FIXME: should not acquire package-names lock
      (add-to-bag-if-found (package-internal-symbols p) string length hash result)
      (add-to-bag-if-found (package-external-symbols p) string length hash result))
    (car result)))

(defun apropos-list (string-designator
                     &optional
                     package-designator
                     external-only
                     &aux (string (the simple-string
                                   (stringify-string-designator string-designator))))
  "Like APROPOS, except that it returns a list of the symbols found instead
  of describing them."
  (if package-designator ; rare to supply this, I suspect
    ;; This loop is extremely inefficient because both DO-SYMBOLS and FIND-SYMBOL
    ;; check for inheritance, which we shouldn't if EXTERNAL-ONLY was given.
    ;; Technically the external-p test could use FIND-EXTERNAL-SYMBOL but portable code
    ;; can't care.  Somebody did, and noticed that it wasn't working, so it got fixed
    ;; in rev e92a2f8844d9 which is ironic because it came from CMUCL, which implemented
    ;; APROPOS differently but then removed the nonstandard option in
    ;; https://gitlab.common-lisp.net/cmucl/cmucl/-/commit/9f652c0515f90b1fc8166d5099a0cee37e76e07c
      (let ((package (find-undeleted-package-or-lose package-designator))
            (result nil))
        (do-symbols (symbol package)
          (when (and (or (not external-only)
                         (and (eq (symbol-package symbol) package)
                              (eq (nth-value 1 (find-symbol (symbol-name symbol)
                                                            package))
                                  :external)))
                     (search string (symbol-name symbol) :test #'char-equal))
            (pushnew symbol result)))
        (return-from apropos-list (sort result #'string-lessp))))
  ;; Since we're going to scan all packages, there are two admissible optimizations:
  ;; * only scan directly present symbols, because each symbol returned has to
  ;;   be in some package.
  ;; * if the table was not modified since core save, as is often the case,
  ;;   then compare by EQ to set of possibly matching strings. This has to be
  ;;   an improvement, because it compares STRING at most once to any symbol-name.
  ;;
  ;; Comparison of (TIME (APROPOS-LIST "str")) in a core with over 300,000 symbols:
  ;;   .840 seconds = baseline
  ;;   .140 seconds = inlining WITH-SYMBOL
  ;;   .104 seconds = MODIFIED check then R/O scan or else WITH-SYMBOL
  (let (candidates)
    (block done
      (sb-vm:map-allocated-objects
       (lambda (obj widetag size)
         (declare (ignore size))
         (cond ((or (= widetag sb-vm:simple-base-string-widetag)
                    #+sb-unicode
                    (= widetag sb-vm:simple-character-string-widetag))
                (when (search string obj :test #'char-equal)
                  (push (cons (calc-symbol-name-hash obj (length obj)) obj) candidates)))
               (t
                (return-from done))))
       :read-only))
    (let ((result (make-hash-table :test 'eq))
          ;; darwin-jit will never take the purified branch. Readonly space exists,
          ;; but contains no symbol names. TUNE-HASHSET-SIZES-OF-ALL-PACKAGES knows that
          ;; and will never reset a table's MODIFIED flag. However, for all other platforms,
          ;; we need to detect if purification happened.
          (core-purified-p (sap> sb-vm:*read-only-space-free-pointer*
                                 (sb-sys:int-sap sb-vm:read-only-space-start))))
      (flet ((find-all-in-table (table)
               (if (and core-purified-p (not (symtbl-modified table)))
                   (dolist (candidate candidates)
                     (let* ((hash (the hash-code (car candidate)))
                            (string (the simple-string (cdr candidate)))
                            (length (length string)))
                       (add-to-bag-if-found table string length hash result)))
                   (dovector (entry (symtbl-cells table))
                     ;; I would have guessed that GETHASH is faster than SEARCH, but if
                     ;; interposed between SYMBOLP and SEARCH, it slows down this loop.
                     ;; That's because almost always the symbol is NOT yet in the result,
                     ;; so an extra GETHASH is a strict increase in the number of
                     ;; instructions executed, for no net reduction in time.
                     (when (and (symbolp entry)
                                (search string (symbol-name entry) :test #'char-equal))
                       (setf (gethash entry result) t))))))
        (do-packages (package) ; FIXME: should not acquire package-names lock
          (find-all-in-table (package-external-symbols package))
          (unless external-only
            (find-all-in-table (package-internal-symbols package)))))
      (sort (loop for k being each hash-key of result collect k) #'string-lessp))))
) ; end FLET

(defun apropos (string-designator &optional package external-only)
  "Briefly describe all symbols which contain the specified STRING.
  If PACKAGE is supplied then only describe symbols present in
  that package. If EXTERNAL-ONLY then only describe
  external symbols in the specified package."
  ;; Implementing this in terms of APROPOS-LIST keeps things simple at the cost
  ;; of some unnecessary consing; and the unnecessary consing shouldn't be an
  ;; issue, since this function is is only useful interactively anyway, and
  ;; we can cons and GC a lot faster than the typical user can read..
  (dolist (symbol (apropos-list string-designator package external-only))
    (briefly-describe-symbol symbol))
  (values))

;;; NOTE: the comments below no longer hold. I can't remember what exactly was broken about
;;; perfect hashing on most packages as long as we downgrade to a hashset on demand.
;;; The FIXME below says it isn't thread-safe, but why not?
;;;
;;; Reorganize both hashsets of all packages, called by SAVE-LISP-AND-DIE.
;;; With a few exceptions, tables are saved at 100% load factor and a perfect hash.
;;; The danger of attempting to achieve such high load using an open-addressing table
;;; is that despite the robinhood algorithm rearranging elements, there could always
;;; be a tremendously bad probe sequence for some symbol, not to mention that unused
;;; cells must be scattered throughout, to assure probing always terminates.
;;; The relevant benchmark is how quickly we can return that a symbol is NOT found.
;;; Test case:
;;; * (defvar *l* (let (l) (do-all-symbols (s l) (push s l))))
;;; * (defun f (list &aux (n 0))
;;;    (dolist (s list n) (if (find-symbol (string s) #.(find-package "CL")) (incf n))))
;;; * (time (f *l*))
;;;
;;; Freshly restarted image after save-lisp-and-die without perfect hash:
;;;   0.017 seconds of real time
;;;
;;; Freshly restarted image with perfect hash:
;;;   0.012 seconds of real time
;;;
(defun tune-hashset-sizes-of-all-packages ()
  (flet ((tune (desired-lf table)
           (resize-symbol-table table (%symtbl-count table) 'intern desired-lf))
         (perfect (table &aux (cells (symtbl-%cells table)))
           (when (functionp (car cells))
             (return-from perfect)) ; already perfectly hashed
           (let* ((cells (cdr cells))
                  (hashes (map '(simple-array (unsigned-byte 32) (*))
                               #'symbol-name-hash
                               (remove-if-not #'symbolp cells)))
                  (hash-expr (sb-c:make-perfect-hash-lambda hashes)))
             (unless hash-expr
               ;; hmm, this seems like it should be a hard error
               ;; since it's confined to the CL package.
               (return-from perfect))
             (let ((fun (compile nil hash-expr))
                   (new-cells (make-array (length hashes) :initial-element 0)))
               (dovector (s cells)
                 (when (symbolp s)
                   (let ((hash (funcall fun (symbol-name-hash s))))
                     (aver (eq (svref new-cells hash) 0))
                     (setf (svref new-cells hash) s))))
               (setf (symtbl-%cells table) (cons fun new-cells)
                     (symtbl-size table) (length hashes)
                     (symtbl-free table) 0
                     (symtbl-deleted table) 0)))))
    (dolist (package (list-all-packages))
      ;; Choose load factor based on whether INTERN is expected at runtime
      ;; FIXME: because changing a package from perfectly hashed back to
      ;; an open-addressing table is not thread-safe, _only_ the CL package can
      ;; become perfectly hashed.
      (let ((lf (cond ((eq (the package package) *keyword-package*) 60/100)
                      ((eq package *cl-package*) 1)
                      (t 8/10)))
            (internals (package-internal-symbols (truly-the package package)))
            (externals (package-external-symbols package)))
        (cond ((= lf 1)
               ;; Should be no internals of CL, don't even bother tuning them
               (perfect externals))
              (t
               (tune lf internals)
               (tune lf externals)))
        ;; The APROPOS-LIST R/O scan optimization is inadmissible if no R/O space
        #-darwin-jit
        (setf (symtbl-modified externals) nil
              (symtbl-modified internals) nil)))))

;;; This function is mainly of interest to developers, should we remove it
;;; from the image?
(export 'show-package-utilization)
(defun show-package-utilization (&aux (tot-ncells 0))
  (flet ((metrics (table &aux (cells (symtbl-%cells table))
                              (reciprocals (car cells))
                              (vec (cdr cells))
                              (nslots (length vec)))
           (when (functionp reciprocals)
             (return-from metrics (values 1 1 1))) ; 1 probe max+avg, 100% load
           (flet ((probe-seq-len (symbol)
                    (let* ((name-hash (symbol-name-hash symbol))
                           (index (symbol-table-hash 1 name-hash nslots))
                           (h2 (symbol-table-hash 2 name-hash nslots))
                           (nprobes 1))
                      (loop (if (eq (svref vec index) symbol) (return nprobes))
                            (setq index (rem (+ index h2) nslots))
                            (incf nprobes)))))
             (let ((nsymbols 0) (max-nprobes 0) (sum-nprobes 0))
               (dovector (symbol (symtbl-cells table))
                 (when (symbolp symbol)
                   (incf nsymbols)
                   (let ((n (probe-seq-len symbol)))
                     (setq max-nprobes (max n max-nprobes))
                     (incf sum-nprobes n))))
               (when (plusp nsymbols)
                 (values max-nprobes
                         (float (/ sum-nprobes nsymbols))
                         (float (/ nsymbols nslots))))))))
  (dolist (pkg (list-all-packages))
    (binding* ((ext (package-external-symbols pkg))
               (int (package-internal-symbols pkg))
               ((ext-max ext-psl ext-lf) (metrics ext))
               ((int-max int-psl int-lf) (metrics int))
               (ncells (+ (length (symtbl-cells int))
                          (length (symtbl-cells ext)))))
      (incf tot-ncells ncells)
      (format t "~8d ~:{~:[~2*~18@t~;~:* ~2d ~6,2f  ~5,1,2f%~] |~} ~a~%"
              ncells
              (list (list ext-max ext-psl ext-lf)
                    (list int-max int-psl int-lf))
              (package-name pkg))))
  (format t "~8d~%" tot-ncells)))

(let ((package (find-package "SB-SEQUENCE")))
  (rename-package package package (list "SEQUENCE")))
