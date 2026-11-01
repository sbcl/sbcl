(in-package :sb-manual)

(defun home-section-name-and-title (name locative)
  (let ((section (ignore-errors (home-section (xref name locative)))))
    (when section
      (values (section-name section)
              (section-title section)))))

;;; Add SB-MANUAL stuff to SB-INT:REFERENCE-CONDITION-REFERENCES.
(defun add-sb-manual-section-to-references (references)
  (let ((new-refs ()))
    (labels ((maybe-add-node (title)
               (when title
                 (push `(:sbcl :node ,title) new-refs)))
             (maybe-add-section (section-name)
               (when section-name
                 (push `(:sbcl :section ,section-name) new-refs)))
             (maybe-add-home-section (name locative)
               (multiple-value-bind (section-name section-title)
                   (home-section-name-and-title name locative)
                 (maybe-add-node section-title)
                 (maybe-add-section section-name))))
      (dolist (reference references (reverse new-refs))
        (push reference new-refs)
        (when (eq (car reference) :sbcl)
          (destructuring-bind (type data) (cdr reference)
            (ecase type
              (:node
               ;; KLUDGE: All sections that we actually reference
               ;; have names that are a trivial transformation of
               ;; the title. -- MG, 2026-11-01.
               (maybe-add-section
                (find-symbol (format nil "@~A"
                                     (substitute #\- #\Space
                                                 (string-upcase data)))
                             (find-package "SB-MANUAL"))))
              (:variable
               (maybe-add-home-section data 'variable))
              (:function
               (maybe-add-home-section data 'function)))))))))
