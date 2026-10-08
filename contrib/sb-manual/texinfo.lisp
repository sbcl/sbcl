(in-package :sb-manual)

(defun lookup-method-xref (xref)
  (let ((gf (ignore-errors (fdefinition (xref-name xref)))))
    (when (typep gf 'generic-function)
      (find-method gf (butlast (xref-locative-args xref))
                   (first (last (xref-locative-args xref)))))))

(defun xref-defined-p (xref)
  (let ((name (xref-name xref))
        (locative-type (xref-locative-type xref)))
    (case locative-type
      ((function generic-function)
       (ignore-errors (fdefinition name)))
      ((method)
       (lookup-method-xref xref))
      ((variable)
       (member (sb-int:info :variable :kind name)
               '(:global :special :constant)))
      ((declaration)
       (find name (sb-cltl2:declaration-information 'declaration)))
      ((class structure condition)
       (find-class name nil))
      ((type)
       (sb-ext:defined-type-name-p name))
      (t
       (cond ((eq locative-type (dummy 'macro))
              (ignore-errors (macro-function name)))
             ((or (eq locative-type (dummy 'setf-function))
                  (eq locative-type (dummy 'setf-generic-function)))
              (ignore-errors (fdefinition name)))
             (t
              (assert nil () "Unexpected locative type in ~S."
                      xref)))))))

;;; This is a partial reimplementation of DREF:ARGLIST. We don't
;;; DEFINE-DUMMY DREF:ARGLIST because we don't want USE-PAX to affect
;;; Texinfo output.
(defun %arglist (xref)
  (case (xref-locative-type xref)
    ((package constant variable type structure class condition declaration nil)
     nil)
    (method
     ;; Add the specializers to method arglists. Cargo-culted from
     ;; DREF::ARGLIST* (METHOD (DREF-EXT:METHOD-DREF)).
     (let* ((arglist (sb-mop:method-lambda-list (lookup-method-xref xref)))
            (specializers (first (last (xref-locative-args xref))))
            (n-specializers (length specializers))
            (seen-special-p nil))
       (values (loop for arg in arglist
                     for i upfrom 0
                     do (when (member arg '(&key &optional &rest &aux
                                            &allow-other-keys))
                          (setq seen-special-p t))
                     collect (let ((name (if (and (not seen-special-p)
                                                  (listp arg)
                                                  (= (length arg) 2))
                                             (first arg)
                                             arg)))
                               (if (and (< i n-specializers)
                                        ;; Do not clutter the arglist
                                        ;; with the superfluous T
                                        ;; specializer.
                                        (not (eq (elt specializers i) t)))
                                   (list name (elt specializers i))
                                   name)))
               :specialized)))
    (t
     (let ((name (xref-name xref)))
       (when (symbolp name)
         (multiple-value-bind (ll unknown)
             (sb-introspect:function-lambda-list name)
           (if unknown
               nil
               (values ll :ordinary))))))))

;;; This is PAX::ARGLIST-TO-TREE repurposed for Texinfo.
(defun print-arglist (arglist &optional kind)
  (when kind
    (let ((methodp (eq kind :specialized)))
      (labels
          ((princ-texinfo (&rest args)
             (dolist (arg args)
               (write-string (escape-texinfo (princ-to-string arg)))))
           (prin1-texinfo (&rest args)
             (dolist (arg args)
               (write-string (escape-texinfo (prin1-to-string arg)))))
           (add-arg (arg level)
             (declare (special *nesting-possible-p*))
             (cond ((member arg '(&key &optional &rest &body))
                    (when (member arg '(&key &optional))
                      (setq *nesting-possible-p* nil))
                    (prin1-texinfo arg))
                   ((symbolp arg)
                    (if (keywordp arg)
                        (prin1-texinfo arg)
                        (princ-texinfo arg)))
                   ((atom arg)
                    (prin1-texinfo arg))
                   (*nesting-possible-p*
                    (add-arglist arg (1+ level)))
                   ;; &KEY or &OPTIONAL default values
                   ((<= (length arg) 3)
                    (let ((name (if (consp (first arg))
                                    ;; Find :X in ((:X *X*) 7).
                                    (caar arg)
                                    (first arg))))
                      (cond ((second arg)
                             ;; (X 7 XP) or (X 7) renders as (X 7)
                             (princ "(")
                             (princ-texinfo name)
                             (princ " ")
                             (prin1-texinfo (second arg))
                             (princ ")"))
                            (t
                             ;; (X NIL XP), (X NIL), (X) renders as X
                             (princ-texinfo name)))))
                   (t
                    (prin1-texinfo arg))))
           (add-arglist (arglist level)
             (let ((*nesting-possible-p* (not methodp)))
               (declare (special *nesting-possible-p*))
               (unless (= level 0)
                 (princ "("))
               (loop for i upfrom 0
                     for rest on arglist
                     do (when (eq (first rest) '&aux)
                          (return))
                        (unless (zerop i)
                          (princ " "))
                        (add-arg (car rest) level)
                        ;; Handle (&WHOLE FORM NAME . ARGS) and similar.
                        (unless (listp (cdr rest))
                          (princ " . ")
                          (add-arg (cdr rest) level)))
               (unless (= level 0)
                 (princ ")")))))
        (add-arglist arglist 0)))))

;;; A partial reimplemtation of DREF:DOCSTRING
(defun %docstring (xref)
  (let ((sb-pcl::*normalize-sbcl-docstrings* nil))
    (values (let ((name (xref-name xref))
                  (locative-type (xref-locative-type xref)))
              (case locative-type
                ((function variable declaration)
                 (documentation name locative-type))
                ((generic-function)
                 (documentation name 'function))
                ((method)
                 (documentation (lookup-method-xref xref) t))
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
            (docstring-package xref))))


(defun locative-type-to-texinfo (locative-type)
  (case locative-type
    (function
     (values "Function" "ffindex"))
    (generic-function
     (values "Generic function" "ffindex"))
    (method
     (values "Method" "ffindex"))
    (variable
     (values "Variable" "vvindex"))
    (class
     (values "Class" "ttindex"))
    (condition
     (values "Condition" "ttindex"))
    (structure
     (values "Structure" "ttindex"))
    (type
     (values "Type" "ttindex"))
    (declaration
     (values "Declaration" "ddindex"))
    (t
     (cond
       ((eq locative-type (dummy 'macro))
        (values "Macro" "ffindex"))
       ((eq locative-type (dummy 'setf-function))
        (values "Setf function" "ffindex"))
       ((eq locative-type (dummy 'setf-generic-function))
        (values "Setf generic function" "ffindex"))
       (t
        (assert nil () "Unexpected locative type ~S." locative-type))))))

(defmacro with-texinfo-to-file (file &body body)
  `(call-maybe-with-texinfo-to-file (lambda () ,@body)
                                   ,file))

(defun call-maybe-with-texinfo-to-file (fn file)
  (if file
      (with-open-file (*standard-output* file :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :supersede)
        (format t "@c Generated by the sb-manual contrib. Do not edit.~%~%")
        (funcall fn))
      (funcall fn)))

(defun remove-markup (string)
  (remove #\\ string))

;;; Write the Texinfo for SECTION to *STANDARD-OUTPUT*. When recursing
;;; into child sections, if a section is in PAGES, then emit an
;;; @include and open a new a file for output.
(defun emit-texinfo-for-section (section &key pages (depth 0)
                                 top-level-menus-to-file
                                 top-level-contents-to-file)
  (let ((title (remove-markup (section-title section)))
        (entries (section-entries section)))
    (format t "@node ~A~%" (texinfo-node-id section))
    (write-concept-keys (concept-keys section) *standard-output*)
    (format t "~A ~A~%~%"
            (ecase depth
              (0 "@top")
              (1 "@chapter")
              (2 "@section")
              (3 "@subsection")
              (4 "@subsubsection"))
            title)
    ;; Generate the @menu
    (let ((child-sections
            (loop for entry in entries
                  when (and (not (stringp entry))
                            (eq (xref-locative-type entry) (dummy 'section)))
                    collect (symbol-value (xref-name entry)))))
      (when child-sections
        (unless top-level-menus-to-file
          (format t "@menu~%"))
        (with-texinfo-to-file top-level-menus-to-file
          (dolist (child-section child-sections)
            (format t "* ~A: ~A.~%"
                    (remove-markup (section-title child-section))
                    (texinfo-node-id child-section))))
        (unless top-level-menus-to-file
          (format t "@end menu~%~%"))))
    ;; Generate the documentation
    (let ((*package* (section-package section)))
      (with-texinfo-to-file top-level-contents-to-file
        (dolist (entry entries)
          (cond ((stringp entry)
                 ;; KLUDGE: @SBCL-MANUAL has an extra docstring that's
                 ;; pretty much the same as @copying in
                 ;; doc/manual/sbcl.texinfo. Skip it.
                 (unless top-level-contents-to-file
                   (emit-texinfo-for-docstring entry)
                   (format t "~%")))
                (t
                 (if (not (eq (xref-locative-type entry) (dummy 'section)))
                     (emit-texinfo-for-definition entry)
                     (let ((page (find (xref-name entry) pages
                                       :key #'first)))
                       (when page
                         (format t "@include ~A~%" (second page)))
                       (with-texinfo-to-file (second page)
                         (emit-texinfo-for-section
                          (symbol-value (xref-name entry))
                          :pages pages
                          :depth (1+ depth))))))))))))

(defun emit-texinfo-for-definition (xref)
  (if (not (xref-defined-p xref))
      (warn "~@<Not documenting ~S because it is not defined.~:@>" xref)
      (multiple-value-bind (docstring *package*) (%docstring xref)
        (multiple-value-bind (type index)
            (locative-type-to-texinfo (xref-locative-type xref))
          (let* ((name (xref-name xref))
                 (*print-case* :downcase)
                 ;; For e.g. #'print
                 (*print-pretty* t)
                 ;; The arglist must be on the @deffn line.
                 (*print-right-margin* most-positive-fixnum))
            (format t "@anchor{~A ~A ~A}~%" type
                    (string-downcase (package-name (symbol-package name)))
                    (string-downcase (symbol-name name)))
            ;; E.g. @vvindex @sortas{save-hooks* sb-ext} *save-hooks* [sb-ext]
            (let ((symbol-name (string-downcase (symbol-name name)))
                  (symbol-package-name
                    (string-downcase (package-name (symbol-package name)))))
              (format t "@~A @sortas{~A ~A} ~A [~A]~%"
                      index
                      (sort-as-name symbol-name)
                      (sort-as-name symbol-package-name)
                      symbol-name
                      symbol-package-name))
            ;; Since we took indexing into our own hands, we just use
            ;; @deffn for all definitions. We could also use @defblock and
            ;; @defline.
            (format t "@deffn{~A} ~A"
                    ;; E.g. "Variable"
                    type
                    (let ((*package* (find-package :cl)))
                      (prin1-to-string name)))
            (multiple-value-bind (arglist arglist-kind)
                (%arglist xref)
              (when arglist
                (princ " ")
                (let ((*package* (find-package :cl)))
                  (print-arglist arglist arglist-kind)))
              (terpri)
              (when docstring
                (emit-texinfo-for-docstring docstring arglist)))
            (format t "@end deffn~%"))))))

;;; Remove leading non-alphanumeric characters. They are not important
;;; when sorting names into indices.
(defun sort-as-name (name)
  (subseq name (or (position-if #'alphanumericp name) 0)))

(defun emit-texinfo-for-docstring (docstring &optional lambda-list)
  (markdown-to-texinfo (reindent-docstring docstring)
                       :lambda-list lambda-list))


;;; Currently, we have the Texinfo file under version control to keep
;;; a closer eye on the Markdown-to-Texinfo converter, which is young.
;;; When that's no longer the case, this is no longer needed.
(defparameter *pages*
  '((@support-and-bugs "support-and-bugs.texinfo")
    (@introduction "intro.texinfo")
    (@starting-and-stopping "start-stop.texinfo")
    (@compiler "compiler.texinfo")
    (@debugger "debugger.texinfo")
    (@efficiency "efficiency.texinfo")
    (@beyond-the-ansi-standard "beyond-ansi.texinfo")
    (@external-formats "external-formats.texinfo")
    (@foreign-function-interface "ffi.texinfo")
    (@pathnames "pathnames.texinfo")
    (@streams "streams.texinfo")
    (@package-locks "package-locks.texinfo")
    (@threading "threading.texinfo")
    (@timers "timers.texinfo")
    (@networking "../../contrib/sb-bsd-sockets/sb-bsd-sockets.texinfo")
    (@profiling "profiling.texinfo")
    (@statistical-profiler "../../contrib/sb-sprof/sb-sprof.texinfo")
    (@contributed-modules "contrib-modules.texinfo")
    (@sb-aclrepl "../../contrib/sb-aclrepl/sb-aclrepl.texinfo")
    (@sb-concurrency "../../contrib/sb-concurrency/sb-concurrency.texinfo")
    (@sb-cover "../../contrib/sb-cover/sb-cover.texinfo")
    (@sb-grovel "../../contrib/sb-grovel/sb-grovel.texinfo")
    (@sb-introspect "../../contrib/sb-introspect/sb-introspect.texinfo")
    (@sb-manual "../../contrib/sb-manual/sb-manual.texinfo")
    (@sb-md5 "../../contrib/sb-md5/sb-md5.texinfo")
    (@sb-posix "../../contrib/sb-posix/sb-posix.texinfo")
    (@sb-queue "../../contrib/sb-queue/sb-queue.texinfo")
    (@sb-rotate-byte "../../contrib/sb-rotate-byte/sb-rotate-byte.texinfo")
    (@sb-sb-simd "../../contrib/sb-simd/sb-simd.texinfo")
    (@sb-simple-streams
     "../../contrib/sb-simple-streams/sb-simple-streams.texinfo")
    (@deprecation "deprecation.texinfo")))

(defun generate-texinfo ()
  (let ((*default-pathname-defaults*
          (truename (merge-pathnames
                     "../../doc/manual/"
                     sb-sys::*sbcl-homedir-pathname*))))
    (with-texinfo-to-file "variables.texinfo"
      (format t "@set VERSION ~A~%~
                 @set UPDATE-MONTH ~A~%"
              (lisp-implementation-version)
              (documentation-generation-date-string)))
    ;; We redirect most lines via *PAGES*, :TOP-LEVEL-MENUS-TO-FILE,
    ;; :TOP-LEVEL-CONTENTS-TO-FILE. Silence the rest, which are not
    ;; needed, as sbcl.texinfo only needs the includes.
    (let ((*standard-output* (make-broadcast-stream)))
      (emit-texinfo-for-section
       (symbol-value '@sbcl-manual) :pages *pages*
       :top-level-menus-to-file "sbcl-menu.texinfo"
       :top-level-contents-to-file "sbcl-contents.texinfo"))))

#+nil
(generate-texinfo)

#+nil
(emit-texinfo-for-section @sb-aclrepl)
#+nil
(emit-texinfo-for-section @starting-and-stopping)
