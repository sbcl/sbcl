(in-package :sb-manual)

(defun emit-plain-for-section (section &key (depth 0))
  (let ((title (remove-markup (section-title section)))
        (entries (section-entries section)))
    (format t "~A ~A~%~%" (make-string (1+ depth) :initial-element #\#) title)
    (let ((*package* (section-package section)))
      (dolist (entry entries)
        (cond ((stringp entry)
               (emit-plain-for-docstring entry)
               (format t "~%"))
              (t
               (if (not (eq (xref-locative-type entry) (dummy 'section)))
                   (emit-plain-for-definition entry)
                   (emit-plain-for-section (symbol-value (xref-name entry))
                                           :depth (1+ depth)))))))))

(defun emit-plain-for-definition (xref)
  (if (not (xref-defined-p xref))
      (warn "~@<Not documenting ~S because it is not defined.~:@>" xref)
      (multiple-value-bind (docstring *package*) (%docstring xref)
        (let* ((type (locative-type-to-texinfo (xref-locative-type xref)))
               (name (xref-name xref))
               (*print-case* :downcase)
               ;; For e.g. #'print
               (*print-pretty* t)
               ;; The arglist must be on the @deffn line.
               (*print-right-margin* most-positive-fixnum))
          (format t "- [~A] ~A"
                  ;; E.g. "Variable"
                  type
                  (let ((*package* (find-package :cl)))
                    (prin1-to-string name)))
          (multiple-value-bind (arglist arglist-kind)
              (%arglist xref)
            (when arglist
              (princ " ")
              (let ((*package* (find-package :cl))
                    ;; KLUDGE: PRINT-ARGLIST calls ESCAPE-TEXINFO.
                    (*texinfo-special-chars* ""))
                (print-arglist arglist arglist-kind)))
            (terpri)
            (terpri)
            (when docstring
              (emit-plain-for-docstring docstring 4)))
          (terpri)))))

(defun emit-plain-for-docstring (docstring &optional (indent 0))
  (write-string (prefix-lines (make-string indent :initial-element #\Space)
                              (sb-pcl::markdown-to-plain-text
                               (reindent-docstring docstring)))))

;;; Per CLHS 11.1.2.1.2 #19, users are not allowed to define methods
;;; like this.
(defmethod documentation :around ((x list) (doc-type (eql t)))
  ;; Newer PAX defines its own DOCUMENTATION method, so we only need
  ;; to handle the no-PAX case.
  (if (lazy-sb-manual-section-p x)
      (with-output-to-string (*standard-output*)
        (emit-plain-for-section x))
      (call-next-method)))

;;; This is a bit heuristic, but it's only for the above DOCUMENTATION
;;; method.
(defun lazy-sb-manual-section-p (object)
  (and (sb-c::proper-list-p object)
       (= (length object) 2)
       (let ((x (first object)))
         (and (sb-c::proper-list-p x)
              (= (length x) 4)
              (eq (first x) :%pax-lazy-doc)
              (eq (third x) :section)
              ;; RESOLVE-LAZY-DOC is unexported.
              (eq (fourth x) 'resolve-lazy-doc)
              (let ((symbol (second x)))
                (and (symbolp symbol)
                     (eq (symbol-package symbol)
                         #.(find-package '#:sb-manual))))))))
