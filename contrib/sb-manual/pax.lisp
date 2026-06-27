;;;; PAX stubs
;;;;
;;;; Contribs cannot depend on external libraries, so we fake as much
;;;; of PAX and DRef as necessary. USE-PAX switches to the real
;;;; implementation.
;;;;
;;;; If PAX is not loaded, the dummy DEFSECTION below still gives us
;;;; the ability to use M-. on section names in docstrings as they are
;;;; just variables, which makes navigating the documentation faster.
;;;;
;;;; When PAX is loaded, we have PAX::@BROWSING-LIVE-DOCUMENTATION for
;;;; low-latency, interactive documentation work and
;;;; PAX::@GENERATING-DOCUMENTATION for autolinked documentation
;;;; (also, see make-pax-docs.sh and http://fixnum.com).
;;;;
;;;; For browsing, use this setup:
;;;;
;;;;     (setq pax:*document-downcase-uppercase-code* t
;;;;           pax:*browse-context* :pax-world)

(in-package :sb-manual)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *using-pax* nil)
  ;; A list of (LOCAL-SYMBOL PACKAGE) elements. Originally,
  ;; LOCAL-SYMBOL has home package SB-MANUAL. For example, the element
  ;; (SECTION :PAX) causes PAX:SECTION to be SHADOWING-IMPORTed in
  ;; USE-PAX.
  (defvar *dummies* ()))

(defmacro defun-dummy ((name package) lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew '(,name ,package) *dummies* :test #'equal)
     (declaim (notinline ,name))
     (unless *using-pax*
       (defun ,name ,lambda-list ,@body))))

(defmacro defmacro-dummy ((name package) lambda-list &body body)
  (unless *using-pax*
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew '(,name ,package) *dummies* :test #'equal)
       (unless *using-pax*
         (defmacro ,name ,lambda-list ,@body)))))

(defparameter *extra-dummies*
  '((argument :pax)
    (macro :dref)
    (setf-function :dref)
    (setf-generic-function :dref)
    (section :pax)
    (concept :pax)
    (clhs :pax)
    (docstring :dref)))

(defun dummy (symbol)
  (let ((*package* (find-package :sb-manual)))
    (read-from-string (symbol-name symbol))))

;;; We might want to populate this with GUESS-PACKAGE-FROM-ARGLIST.
(defvar *definition-to-docstring-package*)
(defvar *package-to-docstring-package*)

(defun resolve-lazy-doc (&rest args)
  (declare (ignore args))
  (use-pax))

(defun lazy-doc-name-p (symbol &optional kind)
  (when (boundp symbol)
    (let ((value (symbol-value symbol)))
      (and (listp value)
           (listp (first value))
           (eq (caar value) :%pax-lazy-doc)
           (or (null kind)
               (eq (third (first value)) kind))))))

(defun use-pax ()
  "Ensure that exported variables are `PAX:SECTION`s.
  It is an error if the `MGL-PAX` library is not loaded.

  Calling this function explicitly is rarely necessary because it is
  called automatically:

  - when `SB-MANUAL` is loaded, if PAX is present;

  - when `PAX:DOCUMENT` (more precisely, `DREF:LOCATE`) is called on
    an `SB-MANUAL` section.

  The latter feature requires v0.4.12 of PAX. See the `MGL-PAX`
  asdf:system."
  (unless *using-pax*
    (assert (find-package '#:mgl-pax) ()
            "The MGL-PAX package does not exist. Load PAX first.")
    ;; Replace dummies with the real symbols.
    (loop for (name package) in (append *dummies* *extra-dummies*)
          do (let ((new-symbol (read-from-string
                                (format nil "~A::~A" package name))))
               (when (and (fboundp new-symbol)
                          (null (macro-function new-symbol)))
                 (setf (fdefinition name) (fdefinition new-symbol)))
               (shadowing-import new-symbol :sb-manual)))
    ;; Ensure PAX:DEFSECTION exports only SECTIONs.
    (eval-string
     "(defmethod pax:exportable-reference-p
          ((package (eql (find-package 'sb-manual)))
           symbol locative-type locative-args)
        (eq locative-type 'section))")
    ;; Reevaluate DEFSECTION and DEFINE-CONCEPT forms with PAX.
    (do-symbols (symbol :sb-manual)
      (when (lazy-doc-name-p symbol)
        (let ((value (symbol-value symbol))
              (source-location
                (sb-int:info :source-location :variable symbol)))
          (eval (subst-dummies (second value)))
          (setf (sb-int:info :source-location :variable symbol)
                source-location))))
    (convert-docstring-package-overrides-to-pax)
    (eval-string
     "(pax:register-doc-in-pax-world
       'sb-manual:@sbcl-manual @sbcl-manual
       `((:objects
          (, @sbcl-manual)
          :source-uri-fn ,(pax:make-git-source-uri-fn
                           nil \"https://github.com/sbcl/sbcl\"
                           :git-root (asdf:system-relative-pathname
                                      :sb-manual \"../../../\")))))")
    (setq *using-pax* t)))

;;; Convert *DEFINITION-TO-DOCSTRING-PACKAGE* to
;;; DREF:DEFINITION-PROPERTIES and *PACKAGE-TO-DOCSTRING-PACKAGE* to
;;; DREF:DEFINITION-PROPERTIES. See DREF-EXT:DOCSTRING*.
(defun convert-docstring-package-overrides-to-pax ()
  (loop for ((name locative) package) in *definition-to-docstring-package*
        do (eval-format
            "(setf (dref-ext:definition-property (dref:xref '~S '~S)~
                                                 'docstring)
                   (list nil (find-package ~S)))"
            name (subst-dummies locative) package))
  (setq *definition-to-docstring-package*
        (subst-dummies *definition-to-docstring-package*))
  (loop for (from-package to-package) in *package-to-docstring-package*
        do (eval-format
            "(setf (dref-ext:definition-property `(:package ,(find-package ~S))
                                                 'docstring)
                   (list nil (find-package ~S)))"
            from-package to-package)))

(defun eval-string (string)
  (let ((*package* (find-package :sb-manual)))
    (eval (read-from-string string))))

(defun eval-format (format-control &rest format-args)
  (let ((*package* (find-package :sb-manual)))
    (eval (read-from-string (apply #'format nil format-control format-args)))))

(defun subst-dummies (tree)
  (let ((new-tree tree))
    (loop
      for (name package) in (append *dummies* *extra-dummies*)
      do (let ((new-name (read-from-string
                          (format nil "~A::~A" package name))))
           (setq new-tree (subst new-name name new-tree))))
    new-tree))


;;; The main job DEFSECTION is to associate a title and a list of
;;; "entries" with a name. An entry is either a docstring or a
;;; reference to a definition of the form (<NAME> <DEFINITION-TYPE>).
;;;
;;; Concepts and index keys may also be used in DEFSECTION forms:
;;;
;;;     (defsection @the-repl (:title "The REPL" :concepts (@repl))
;;;        "The REPL is ...")
;;;
;;; When processed, this becomes
;;;
;;;    @cindex Read-Eval-Print Loop
;;;    @cindex REPL
;;;    @node the repl
;;;    @section The REPL
;;;    The REPL is ...
;;;
;;; Note that it doesn't matter in :KEYS of DEFSECTION whether a
;;; concept is pure or titled.
;;;
;;; Note that referencing sections in docstrings does not cause
;;; indexing. This is due to examples like
;;;
;;;     We now move on to the next section, @XXX.
;;;     See @XXX, for other considerations.
;;;
;;; In general, mentioning and linking a section is not the same as
;;; being about it, and only the latter deserves a concept index
;;; entry.
;;;
;;; See also PAX:DEFSECTION.
(defmacro-dummy (defsection pax)
                (name (&key (package *package*) (export t) title concepts)
                      &body entries)
  (let ((value
          `((:%pax-lazy-doc ,name :section resolve-lazy-doc)
            (defsection ,name (:package ,package :export ,export :title ,title
                               :concepts ,concepts)
              ,@entries))))
    `(progn
       (defparameter ,name ',value)
       ,@(when export
           `((export ',name :sb-manual))))))

(defun-dummy (section-name :pax) (section)
  (second (second section)))

(defun-dummy (section-title :pax) (section)
  (getf (third (second section)) :title))

(defun-dummy (concept-keys :pax) (section)
  (resolve-concept-symbols (getf (third (second section)) :concepts)))

(defun-dummy (section-package :pax) (section)
  (find-package (getf (third (second section)) :package)))

;;; This is a list of (NAME LOCATIVE) elements with our dummy DEFSECTION.
(defun-dummy (section-entries :pax) (section)
  (nthcdr 3 (second section)))

(defun-dummy (xref-name :dref) (xref)
  (first xref))

(defun-dummy (xref-locative :dref) (xref)
  (normalize-locative (second xref)))

(defun normalize-locative (locative)
  (if (and (listp locative)
           (null (cdr locative)))
      (first locative)
      locative))

(defun-dummy (xref-locative-type :dref) (xref)
  (first (sb-c::ensure-list (second xref))))


;;; DEFINE-CONCEPT names a set of index keys. When MARKDOWN-TO-TEXINFO
;;; encounters the name of a concept in a docstring, it emits Texinfo
;;; `@cindex' lines for its keys. There are two kinds of concepts.
;;;
;;; When a `pure' concept (that has no title) is processed in a
;;; docstring by MARKDOWN-TO-TEXINFO, it produces no visible output,
;;; but it emits Texinfo @cindex lines for its KEYS. It is thus _not_
;;; part of the normal flow of text.
;;;
;;;     (define-concept ~repl (:keys ("Read-Eval-Print Loop" "REPL")))
;;;
;;; Example use:
;;;
;;;     The REPL ~REPL is interactive.
;;;
;;; This becomes
;;;
;;;     @cindex Read-Eval-Print Loop
;;;     @cindex REPL
;;;     The REPL  is interactive.
;;;
;;; When a `titled' concept is processed, it is replaced by its title.
;;; Use this in the normal flow of text like section names. Note that
;;; other concepts can be referenced in KEYS. The definition below is
;;; the titled synonym of ~REPL.
;;;
;;;     (define-concept @repl (:title "REPL" :keys (~repl)))
;;;
;;; Example use:
;;;
;;;     The @REPL is interactive.
;;;
;;; This becomes
;;;
;;;     @cindex Read-Eval-Print Loop
;;;     @cindex REPL
;;;     The REPL is interactive.
;;;
;;; By convention, names of pure concepts start with #\~, while those
;;; of titled concepts start with the usual #\@. The tilde is to draw
;;; attention to that the name is not part of the normal text.
;;;
;;; Finally, index keys can be hierarchical:
;;;
;;;     (define-concept ~defining-macros (:keys (("defining" "macros")
;;;                                              ("macros," "defining"))))
;;;
;;; This is processed into
;;;
;;;     @cindex defining @subentry macros
;;;     @cindex macros, @subentry defining
;;;
;;; Note Texinfo's @sortas is not supported yet. PAX implements that
;;; by allowing any subkey string to be a (<NAME> . <SORT-AS-STRING>)
;;; cons.
;;;
;;; See also PAX:DEFINE-CONCEPT.
(defmacro-dummy (define-concept pax) (name (&key title keys))
  (let ((value
          `((:%pax-lazy-doc ,name :concept resolve-lazy-doc)
            (define-concept ,name (:title ,title :keys ,keys)))))
    `(defparameter ,name ',value)))

(defun-dummy (multiplexing-concept-keys :pax) (concept)
  (resolve-concept-symbols (getf (third (second concept)) :keys)))

(defun-dummy (doctitle :pax) (concept)
  (getf (third (second concept)) :title))

(defun resolve-concept-symbols (list)
  (if (find-if #'symbolp list)
      (loop for x in list
            append (if (symbolp x)
                       (multiplexing-concept-keys (find-concept x))
                       (list x)))
      list))

(defun find-concept (name)
  (if (doc-name-p name :concept)
      (symbol-value name)
      (error "Undefined ~S ~S." 'concept name)))
