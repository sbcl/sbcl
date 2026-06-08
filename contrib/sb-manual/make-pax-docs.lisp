;;;; Generate the SBCL manual in various formats in doc/manual/ with PAX

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :mgl-pax/full)
  (require :sb-manual))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-manual::use-pax))

(in-package :sb-manual)

(defvar *git-forge-uri*)
(defvar *git-root*)
(defvar *output-dir*)

(defvar *directory* (truename (make-pathname :name nil :type nil
                                             :defaults *load-truename*)))

(defun sbcl-pages* (format)
  (let ((source-uri-fn (when (and (not (eq format :plain))
                                  *git-forge-uri*)
                         (pax:make-git-source-uri-fn nil *git-forge-uri*
                                                     :git-root *git-root*)))
        (output-file (ecase format
                       ((:plain) "sbcl-manual.txt")
                       ((:markdown) "sbcl-manual.md")
                       ((:pdf) "sbcl-manual.pdf")
                       ((:html) "html/sbcl-manual.html"))))
    `((:objects (, @sbcl-manual)
       :output (,(merge-pathnames output-file *output-dir*)
                :if-does-not-exist :create
                :if-exists :supersede
                ,@(when (eq format :pdf)
                    '(:element-type (unsigned-byte 8)))
                :ensure-directories-exist t)
       ,@(when source-uri-fn
           `(:source-uri-fn ,source-uri-fn))))))

;;; Adjust the width of section numbers in the PDF table of contents, so
;;; that e.g "17.10.13" doesn't protude from its box.
(defparameter *pandoc-pdf-adjusted-table-of-contents
  "\\makeatletter
% \\@dottedtocline{level}{indent_before_number}{width_of_number_box}
\\renewcommand*\\l@subsection{\\@dottedtocline{2}{1.5em}{2.8em}}
\\renewcommand*\\l@subsubsection{\\@dottedtocline{3}{4.3em}{4.0em}}
\\makeatother")

(defun make-pax-docs (&optional git-forge-uri)
  (let ((*git-forge-uri* (or (and (plusp (length git-forge-uri))
                                  git-forge-uri)
                             "https://github.com/sbcl/sbcl"))
        (*git-root* (truename (merge-pathnames "../../" *directory*)))
        (*output-dir* (merge-pathnames "output/" *directory*))
        (pax:*document-downcase-uppercase-code* t)
        (pax:*document-url-versions* '(1))
        (pax:*document-pandoc-pdf-header-includes*
          (format nil "~A~%~A~%"
                  pax:*document-pandoc-pdf-header-includes*
                  *pandoc-pdf-adjusted-table-of-contents))
        (pax:*document-pandoc-pdf-options*
          (remove "--verbose" pax:*document-pandoc-pdf-options*
                  :test #'equal)))
    (format t "Git root: ~A~%Git forge URI: ~A~%Output dir: ~A~%"
            *git-root* *git-forge-uri* *output-dir*)
    (format t "Generating manual in plain text format~%")
    (pax:document @sbcl-manual :pages (sbcl-pages* :plain) :format :plain)
    (format t "Generating manual in Markdown format~%")
    (pax:document @sbcl-manual :pages (sbcl-pages* :markdown) :format :markdown)
    (format t "Generating manual in PDF format~%")
    (pax:document @sbcl-manual :pages (sbcl-pages* :pdf) :format :pdf)
    (format t "Generating manual in HTML format~%")
    (pax:update-asdf-system-html-docs @sbcl-manual "sb-manual"
                                      :pages (sbcl-pages* :html)
                                      :target-dir (merge-pathnames "html/"
                                                                   *output-dir*)
                                      :style :charter)))

#+nil
(make-pax-docs)
