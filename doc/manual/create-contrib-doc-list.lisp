;;;; -*- lisp -*-

;;;; "Lisp as scripting language -- discuss"

;;;; Generate contrib-docs.texi-temp from any texinfo files found in
;;;; the contrib/ sub-tree.

(defun nodename (texi-file)
  (with-open-file (f texi-file)
    (loop for line = (read-line f)
         while line
         do (let ((index (search "@node" line)))
              (when index
                (return-from nodename
                  (subseq line (+ index 1 (length "@node"))))))))
  (error "No `@node' line found in file ~A" texi-file))

(let ((texi-files (directory "../../contrib/**/*.texinfo")))
  (with-open-file (out "contrib-doc-list.texi-temp" :direction :output
                       :if-does-not-exist :create :if-exists :supersede)
    (write-string "@c -*- texinfo -*-

@c Include documentation for contrib modules.
@c This is a generated file - do not edit!

" out)
    (write-line "@menu" out)
    (dolist (texi-file texi-files)
      (let ((nodename (nodename texi-file)))
        (format out "* ~A::~%" nodename)))
    (write-line "@end menu" out)
    (terpri out)
    (dolist (texi-file texi-files)
      (format out "@page~%@include ~A~%"
              (namestring (make-pathname
                           :directory (list* :relative :up :up
                                             (last
                                              (pathname-directory texi-file) 2))
                           :name (pathname-name texi-file)
                           :type (pathname-type texi-file)))))))

(sb-ext:quit)
