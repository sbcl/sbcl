;;;; -*- lisp -*-

;;;; A docstring extractor for the sbcl manual.  Creates
;;;; @include-ready documentation from the docstrings of exported
;;;; symbols of specified packages.

;;;; This software is part of the SBCL software system. SBCL is in the
;;;; public domain and is provided with absolutely no warranty. See
;;;; the COPYING file for more information.

;;;; Written by Rudi Schlatte <rudi@constantly.at>


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-introspect))

(defparameter *documentation-types*
  '(compiler-macro
    function
    method-combination
    setf
    ;;structure  ; also handled by `type'
    type
    variable)
  "A list of symbols accepted as second argument of `documentation'")

;;; Collecting info from package

(defun documentation-for-symbol (symbol)
  "Collects all doc for a symbol, returns a list of the
  form (symbol doc-type docstring).  See `*documentation-types*'
  for the possible values of doc-type."
  (loop for kind in *documentation-types*
       for doc = (documentation symbol kind)
       when doc
       collect (list symbol kind doc)))

(defun collect-documentation (package)
  "Collects all documentation for all external symbols of the
  given package, as well as for the package itself."
  (let* ((package (find-package package))
         (package-doc (documentation package t))
         (result nil))
    (check-type package package)
    (do-external-symbols (symbol package)
      (let ((docs (documentation-for-symbol symbol)))
        (when docs (setf result (nconc docs result)))))
    (when package-doc
      (setf result (nconc (list (list (intern (package-name package) :keyword)
                                      'package package-doc)) result)))
    result))

;;; Helpers for texinfo output

(defvar *texinfo-escaped-chars* "@{}"
  "Characters that must be escaped with #\@ for Texinfo.")

(defun texinfoify (string-designator)
  "Return 'string-designator' with characters in
  *texinfo-escaped-chars* escaped with #\@"
  (let ((name (string string-designator)))
    (nstring-downcase
     (with-output-to-string (s)
       (loop for char across name
          when (find char *texinfo-escaped-chars*)
          do (write-char #\@ s)
          do (write-char char s))))))

;;; Begin, rest and end of definition.

(defun argument-list (fname)
  (sb-introspect:function-arglist fname))

(defvar *character-replacements*
  '((#\* . "star") (#\/ . "slash") (#\+ . "plus"))
  "Characters and their replacement names that `alphanumize'
  uses.  If the replacements contain any of the chars they're
  supposed to replace, you deserve to lose.")

(defvar *characters-to-drop* '(#\\ #\` #\')
  "Characters that should be removed by `alphanumize'.")


(defun alphanumize (symbol)
  "Construct a string without characters like *`' that will
  f-star-ck up filename handling.  See `*character-replacements*'
  and `*characters-to-drop*' for customization."
  (let ((name (remove-if #'(lambda (x) (member x *characters-to-drop*))
                         (string symbol)))
        (chars-to-replace (mapcar #'car *character-replacements*)))
    (flet ((replacement-delimiter (index)
             (cond ((or (< index 0) (>= index (length name))) "")
                   ((alphanumericp (char name index)) "-")
                   (t ""))))
      (loop for index = (position-if #'(lambda (x) (member x chars-to-replace))
                                     name)
         while index
         do (setf name (concatenate 'string (subseq name 0 index)
                                    (replacement-delimiter (1- index))
                                    (cdr (assoc (aref name index)
                                                *character-replacements*))
                                    (replacement-delimiter (1+ index))
                                    (subseq name (1+ index))))))
    name))

(defun unique-name (symbol package kind)
  (nstring-downcase
   (format nil "~A-~A-~A"
           (ecase kind
             (compiler-macro "compiler-macro")
             (function (cond
			 ((macro-function symbol) "macro")
			 ((special-operator-p symbol) "special-operator")
			 (t "fun")))
             (method-combination "method-combination")
             (package "package")
             (setf "setf-expander")
             (structure "struct")
             (type (let ((class (find-class symbol nil))))
		     (etypecase class
		       (structure-class "struct")
		       (standard-class "class")
		       (sb-pcl::condition-class "condition")
		       ((or built-in-class null) "type"))))
             (variable (if (constantp symbol)
                           "constant"
                           "var")))
           (package-name package)
           (alphanumize symbol))))

(defun def-begin (symbol kind)
  (ecase kind
    (compiler-macro "@deffn {Compiler Macro}")
    (function (cond
		((macro-function symbol) "@deffn Macro")
		((special-operator-p symbol) "@deffn {Special Operator}")
		(t "@deffn Function")))
    (method-combination "@deffn {Method Combination}")
    (package "@defvr Package")
    (setf "@deffn {Setf Expander}")
    (structure "@deftp Structure")
    (type (let ((class (find-class symbol nil))))
	    (etypecase class
	      (structure-class "@deftp Structure")
	      (standard-class "@deftp Class")
	      (sb-pcl::condition-class "@deftp Condition")
	      ((or built-in-class null) "@deftp Type"))))
    (variable (if (constantp symbol)
                  "@defvr Constant"
                  "@defvr Variable"))))

(defun def-index (symbol kind)
  (case kind
    ((compiler-macro function method-combination)
     (format nil "@findex ~A" (texinfoify symbol)))
    ((structure type)
     (format nil "@tindex ~A" (texinfoify symbol)))
    (variable
     (format nil "@vindex ~A" (texinfoify symbol)))))

(defparameter *arglist-keywords*
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(defun texinfoify-arglist-part (part)
  (with-output-to-string (s)
    (etypecase part
      (string (prin1 (texinfoify part) s))
      (number (prin1 part s))
      (symbol
       (if (member part *arglist-keywords*)
           (princ (texinfoify part) s)
           (format s "@var{~A}" (texinfoify part))))
      (list
       (format s "(~{~A~^ ~})" (mapcar #'texinfoify-arglist-part part))))))

(defun def-arglist (symbol kind)
  (case kind
    (function
     (format nil "~{~A~^ ~}" (mapcar #'texinfoify-arglist-part
                                     (argument-list symbol))))))

(defun def-end (symbol kind)
  (declare (ignore symbol))
  (ecase kind
    ((compiler-macro function method-combination setf) "@end deffn")
    ((package variable) "@end defvr")
    ((structure type) "@end deftp"))
  )

(defun make-info-file (package &optional filename)
  "Create a file containing all available documentation for the
  exported symbols of `package' in Texinfo format.  If `filename'
  is not supplied, a file \"<packagename>.texinfo\" is generated.

  The definitions can be referenced using Texinfo statements like
  @ref{<doc-type>_<packagename>_<symbol-name>.texinfo}.  Texinfo
  syntax-significant characters are escaped in symbol names, but
  if a docstring contains invalid Texinfo markup, you lose."
  (let* ((package (find-package package))
         (filename (or filename (make-pathname
                                 :name (string-downcase (package-name package))
                                 :type "texinfo")))
         (docs (sort (collect-documentation package) #'string< :key #'first)))
    (with-open-file (out filename :direction :output
                         :if-does-not-exist :create :if-exists :supersede)
      (loop for (symbol kind docstring) in docs
           do (format out "~&@anchor{~A}~%~A ~A:~A~@[ ~A~]~%~A~&~A~%~A~%~%"
                      (unique-name symbol package kind)
                      (def-begin symbol kind)
                      (texinfoify (package-name package))
                      (texinfoify symbol)
                      (def-arglist symbol kind)
                      (def-index symbol kind)
                      (texinfoify docstring)
                      (def-end symbol kind))))
    filename))

(defun docstrings-to-texinfo (directory &rest packages)
  "Create files in `directory' containing Texinfo markup of all
  docstrings of each exported symbol in `packages'.  `directory'
  is created if necessary.  If you supply a namestring that
  doesn't end in a slash, you lose.  The generated files are of
  the form \"<doc-type>_<packagename>_<symbol-name>.texinfo\" and
  can be included via @include statements.  Texinfo
  syntax-significant characters are escaped in symbol names, but
  if a docstring contains invalid Texinfo markup, you lose."
  (let ((directory (merge-pathnames (pathname directory))))
    (ensure-directories-exist directory)
    (dolist (package packages)
      (loop
         with docs = (collect-documentation (find-package package))
         for (symbol kind docstring) in docs
         for doc-identifier = (unique-name symbol package kind)
         do (with-open-file (out
                             (merge-pathnames
                              (make-pathname :name doc-identifier :type "texinfo")
                              directory)
                             :direction :output
                             :if-does-not-exist :create :if-exists :supersede)
              (format out "~&@anchor{~A}~%~A ~A:~A~@[ ~A~]~%~A~&~A~%~A~%~%"
                      (unique-name symbol package kind)
                      (def-begin symbol kind)
                      (texinfoify (package-name package))
                      (texinfoify symbol)
                      (def-arglist symbol kind)
                      (def-index symbol kind)
                      (texinfoify docstring)
                      (def-end symbol kind)))))
    directory))
