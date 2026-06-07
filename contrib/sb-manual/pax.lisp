;;;; PAX stubs
;;;;
;;;; Contribs cannot depend on external libraries, so we fake as much
;;;; of PAX and DRef as necessary. SWITCH-TO-PAX switches to the real
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
  (defvar *use-pax* nil)
  ;; A list of (LOCAL-SYMBOL PACKAGE) elements. Originally,
  ;; LOCAL-SYMBOL has home package SB-MANUAL. For example, the element
  ;; (SECTION :PAX) causes PAX:SECTION to be SHADOWING-IMPORTed in
  ;; SWITCH-TO-PAX.
  (defvar *dummies* ()))

(defmacro defun-dummy ((name package) lambda-list &body body)
  (unless *use-pax*
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew '(,name ,package) *dummies* :test #'equal)
       (defun ,name ,lambda-list ,@body))))

(defmacro defmacro-dummy ((name package) lambda-list &body body)
  (unless *use-pax*
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew '(,name ,package) *dummies* :test #'equal)
       (defmacro ,name ,lambda-list ,@body))))

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

(defun switch-to-pax ()
  (unless *use-pax*
    (require 'mgl-pax)
    ;; Replace dummies with the real symbols.
    (let ((dummies (loop for (name package) in (append *dummies*
                                                       *extra-dummies*)
                         collect (format nil "~A:~A" package name))))
      (shadowing-import (mapcar #'read-from-string dummies) :sb-manual))
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
    (setq *use-pax* t)))

;;; Convert *DEFINITION-TO-DOCSTRING-PACKAGE* to
;;; DREF:DEFINITION-PROPERTIES and *PACKAGE-TO-DOCSTRING-PACKAGE* to
;;; DREF:DEFINITION-PROPERTIES. See DREF-EXT:DOCSTRING*.
(defun convert-docstring-package-overrides-to-pax ()
  (loop for ((name locative) package) in *definition-to-docstring-package*
        do (eval-format
            "(setf (dref-ext:definition-property (dref:dref '~S '~S) 'docstring)
                   (list nil (find-package ~S)))"
            name (subst-extras locative) package))
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

(defun-dummy (resolve :dref) (xref)
  (cond ((eq (xref-locative-type xref) 'section)
         (or (ignore-errors (symbol-value (xref-name xref)))
             (assert nil () "Undefined SECTION ~S." (xref-name xref))))
        (t
         (assert nil () "Unexpected locative type in ~S." xref))))

(defun-dummy (arglist :dref) (xref)
  (let ((name (xref-name xref))
        (locative-type (xref-locative-type xref)))
    (lambda-list* name locative-type)))

(defun-dummy (docstring :dref) (xref)
  (values (let ((name (xref-name xref))
                (locative-type (xref-locative-type xref)))
            (case locative-type
              ((function variable)
               (documentation name locative-type))
              ((generic-function)
               (documentation name 'function))
              ((type class structure condition)
               (documentation name 'type))
              (t
               (cond ((eq locative-type (dummy 'macro))
                      (documentation (macro-function name) t))
                     ((eq locative-type (dummy 'setf-function))
                      (documentation (fdefinition name) t))
                     ((eq locative-type (dummy 'setf-generic-function))
                      (documentation (fdefinition name) t))
                     (t
                      (assert nil () "Unexpected locative type in ~S."
                              xref))))))
          ;; To be compatible with PAX::@PACKAGE-AND-READTABLE, we
          ;; always return a non-NIL package.
          (docstring-package xref)))


(defun lambda-list* (name kind)
  (case kind
    ((package constant variable type structure class condition method nil)
     nil)
    (t
     ;; KLUDGE: Eugh.
     ;;
     ;; believe it or not, the above comment was written before CSR
     ;; came along and obfuscated this.  (2005-07-04)
     (when (symbolp name)
       (labels ((clean (x &key optional key)
                  (typecase x
                    (atom x)
                    ((cons (member &optional))
                     (cons (car x) (clean (cdr x) :optional t)))
                    ((cons (member &key))
                     (cons (car x) (clean (cdr x) :key t)))
                    ((cons (member &whole &environment))
                     ;; Skip these
                     (clean (cdr x) :optional optional :key key))
                    ((cons cons)
                     (cons
                      (cond (key (if (consp (caar x))
                                     (caaar x)
                                     (caar x)))
                            (optional (caar x))
                            (t (clean (car x))))
                      (clean (cdr x) :key key :optional optional)))
                    (cons
                     (cons
                      (cond ((or key optional) (car x))
                            (t (clean (car x))))
                      (clean (cdr x) :key key :optional optional))))))
         (multiple-value-bind (ll unknown)
             (sb-introspect:function-lambda-list name)
           (if unknown
               (values nil t)
               (clean ll))))))))
