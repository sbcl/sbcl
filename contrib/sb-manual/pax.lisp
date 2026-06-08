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
;;;; PAX::@GENERATING-DOCUMENTATION for autolinked documentation.

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
    (clhs :pax)))

(defun dummy (symbol)
  (let ((*package* (find-package :sb-manual)))
    (read-from-string (symbol-name symbol))))

;;; We might want to populate this with GUESS-PACKAGE-FROM-ARGLIST.
(defvar *definition-to-docstring-package*)
(defvar *package-to-docstring-package*)

(defun use-pax ()
  (unless *using-pax*
    (assert (find-package '#:mgl-pax))
    ;; Replace dummies with the real symbols.
    (loop for (name package) in (append *dummies* *extra-dummies*)
          do (let ((new-symbol (read-from-string
                                (format nil "~A:~A" package name))))
               (when (and (fboundp new-symbol)
                          (null (macro-function new-symbol)))
                 (setf (fdefinition name) (fdefinition new-symbol)))
               (shadowing-import new-symbol :sb-manual)))
    ;; Arrange for that only SECTIONs will be exported by
    ;; PAX:DEFSECTION.
    (eval-string
     "(defmethod pax:exportable-reference-p
          ((package (eql (find-package 'sb-manual)))
           symbol locative-type locative-args)
        (eq locative-type 'section))")
    ;; Reevaluate DEFSECTION forms with PAX.
    (do-external-symbols (symbol :sb-manual)
      (when (and (char= #\@ (aref (symbol-name symbol) 0))
                 (boundp symbol))
        (let ((value (symbol-value symbol)))
          (assert (listp value))
          (assert (eq (first value) 'defsection))
          (assert (eq (second value) symbol))
          (let ((source-location
                  (sb-int:info :source-location :variable symbol)))
            (eval `(,(read-from-string "pax:defsection")
                    ,@(subst-extras (rest value))))
            (setf (sb-int:info :source-location :variable symbol)
                  source-location)))))
    (convert-docstring-package-overrides-to-pax)
    ;; FIXME: register doc?
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
            name (subst-extras locative) package))
  (setq *definition-to-docstring-package*
        (subst-extras *definition-to-docstring-package*))
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

(defun subst-extras (tree)
  (let ((new-tree tree))
    (loop
      for (name package) in *extra-dummies*
      do (let ((new-name (read-from-string (format nil "~A:~A" package name))))
           (setq new-tree (subst new-name name new-tree))))
    new-tree))


(defmacro-dummy (defsection pax)
                (name (&key (package *package*) (export t) title)
                      &body entries)
  (let ((defsection-form
          `(defsection ,name (:package ,package :export ,export :title ,title)
             ,@entries)))
    `(progn
       (defparameter ,name ',defsection-form)
       ,@(when export
           `((export ',name :sb-manual))))))

(defun-dummy (section-name :pax) (section)
  (second section))

(defun-dummy (section-title :pax) (section)
  (getf (third section) :title))

(defun-dummy (section-package :pax) (section)
  (find-package (getf (third section) :package)))

;;; This is a list of (NAME LOCATIVE) elements with our dummy DEFSECTION.
(defun-dummy (section-entries :pax) (section)
  (nthcdr 3 section))

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
