;;; -*- lisp -*-

;;;; A docstring extractor for the sbcl manual.  Creates
;;;; @include-ready documentation from the docstrings of exported
;;;; symbols of specified packages.


;;;; This software is part of the SBCL software system. SBCL is in the
;;;; public domain and is provided with absolutely no warranty. See
;;;; the COPYING file for more information.
;;;;
;;;; Written by Rudi Schlatte <rudi@constantly.at>


;;;; Formatting heuristics (tweaked to format SAVE-LISP-AND-DIE sanely):
;;;;
;;;; Formats SYMBOL as @code{symbol}, or @var{symbol} if symbol is in
;;;; the argument list of the defun / defmacro.
;;;;
;;;; Lines starting with * or - that are followed by intented lines
;;;; are marked up with @itemize.
;;;;
;;;; Lines containing only a SYMBOL that are followed by indented
;;;; lines are marked up as @table @code, with the SYMBOL as the item.



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

(defun texinfoify (string-designator &optional (downcase-p t))
  "Return 'string-designator' with characters in
  *texinfo-escaped-chars* escaped with #\@.  Optionally downcase
  the result."
  (let ((result (with-output-to-string (s)
       (loop for char across (string string-designator)
          when (find char *texinfo-escaped-chars*)
          do (write-char #\@ s)
          do (write-char char s)))))
    (if downcase-p (nstring-downcase result) result)))

(defvar *symbol-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ*:-+"
  "List of characters that make up symbols in a docstring.")

(defvar *symbol-delimiters* " ,.!?;")

(defun locate-symbols (line)
  "Return a list of index pairs of symbol-like parts of LINE."
  ;; This would be a good application for a regex ...
  (do ((result nil)
       (begin nil)
       (maybe-begin t)
       (i 0 (1+ i)))
      ((= i (length line))
       ;; symbol at end of line
       (when (and begin (or (> i (1+ begin))
                            (not (member (char line begin) '(#\A #\I)))))
         (push (list begin i) result))
       (nreverse result))
    (cond
      ((and begin (find (char line i) *symbol-delimiters*))
       ;; symbol end; remember it if it's not "A" or "I"
       (when (or (> i (1+ begin)) (not (member (char line begin) '(#\A #\I))))
         (push (list begin i) result))
       (setf begin nil
             maybe-begin t))
      ((and begin (not (find (char line i) *symbol-characters*)))
       ;; Not a symbol: abort
       (setf begin nil))
      ((and maybe-begin (not begin) (find (char line i) *symbol-characters*))
       ;; potential symbol begin at this position
       (setf begin i
             maybe-begin nil))
      ((find (char line i) *symbol-delimiters*)
       ;; potential symbol begin after this position
       (setf maybe-begin t))
      (t
       ;; Not reading a symbol, not at potential start of symbol
       (setf maybe-begin nil)))))

(defun all-symbols (list)
  (cond ((null list) nil)
        ((symbolp list) (list list))
        ((consp list) (append (all-symbols (car list))
                              (all-symbols (cdr list))))
        (t nil)))


(defun frob-doc-line (line var-symbols)
  "Format symbols in LINE texinfo-style: either as code or as
  variables if the symbol in question is contained in
  var-symbols."
  (with-output-to-string (result)
    (let ((last 0))
      (dolist (symbol-index (locate-symbols line))
        (write-string (subseq line last (first symbol-index)) result)
        (let ((symbol-name (apply #'subseq line symbol-index)))
          (format result (if (member symbol-name var-symbols
                                     :test #'string=)
                             "@var{~A}"
                             "@code{~A}")
                  (string-downcase symbol-name)))
        (setf last (second symbol-index)))
      (write-string (subseq line last) result))))

(defparameter *itemize-start-characters* '(#\* #\-)
  "Characters that might start an itemization in docstrings when
  at the start of a line.")

(defun indentation (line)
  "Position of first non-SPACE character in LINE."
  (position-if-not (lambda (c) (char= c #\Space)) line))

(defun maybe-itemize-offset (line)
  "Return NIL or the indentation offset if LINE looks like it
  starts an item in an itemization."
  (let ((offset (indentation line)))
    (when (and offset
               (member (char line offset) *itemize-start-characters*
                       :test #'char=))
      offset)))

(defun collect-maybe-itemized-section (lines starting-line arglist-symbols)
  ;; Return index of next line to be processed outside
  (let ((this-offset (maybe-itemize-offset (svref lines starting-line)))
        (result nil)
        (lines-consumed 0))
    (loop for line-number from starting-line below (length lines)
       for line = (svref lines line-number)
       for indentation = (indentation line)
       for offset = (maybe-itemize-offset line)
       do (cond
            ((not indentation)
             ;; empty line -- inserts paragraph.
             (push "" result)
             (incf lines-consumed))
            ((and offset (> indentation this-offset))
             ;; nested itemization -- handle recursively
             (multiple-value-bind (sub-lines-consumed sub-itemization)
                 (collect-maybe-itemized-section lines line-number
                                                 arglist-symbols)
               (when sub-lines-consumed
                 (incf line-number (1- sub-lines-consumed)) ; +1 on next loop
                 (incf lines-consumed sub-lines-consumed)
                 (setf result (nconc (nreverse sub-itemization) result)))))
            ((and offset (= indentation this-offset))
             ;; start of new item
             (push (format nil "@item ~A"
                           (frob-doc-line (subseq line (1+ offset))
                                          arglist-symbols))
                   result)
             (incf lines-consumed))
            ((and (not offset) (> indentation this-offset))
             ;; continued item from previous line
             (push (frob-doc-line line arglist-symbols) result)
             (incf lines-consumed))
            (t
             ;; end of itemization
             (loop-finish))))
    (if
     ;; a single-line itemization isn't.
     (> (count-if (lambda (line) (> (length line) 0)) result) 1)
     (values lines-consumed
             `("@itemize" ,@(reverse result) "@end itemize"))
     nil)))


(defun maybe-table-offset (line)
  "Return NIL or the indentation offset if LINE looks like it
  starts an item in a tabulation, i.e., there's only a symbol on the line."
  (let ((offset (indentation line)))
    (when (and offset
               (every (lambda (c)
                        (or (char= c #\Space)
                            (find c *symbol-characters* :test #'char=)))
                      line))
      offset)))

(defun collect-maybe-table-section (lines starting-line arglist-symbols)
  ;; Return index of next line to be processed outside
  (let ((this-offset (maybe-table-offset (svref lines starting-line)))
        (result nil)
        (lines-consumed 0))
    (loop for line-number from starting-line below (length lines)
       for line = (svref lines line-number)
       for indentation = (indentation line)
       for offset = (maybe-table-offset line)
       do (cond
            ((not indentation)
             ;; empty line -- inserts paragraph.
             (push "" result)
             (incf lines-consumed))
            ((and offset (= indentation this-offset))
             ;; start of new item, or continuation of previous item
             (if (and result (search "@item" (car result) :test #'char=))
                 (push (format nil "@itemx ~A"
                               (frob-doc-line line arglist-symbols))
                       result)
                 (progn
                   (push "" result)
                   (push (format nil "@item ~A"
                                 (frob-doc-line line arglist-symbols))
                         result)))
             (incf lines-consumed))
            ((> indentation this-offset)
             ;; continued item from previous line
             (push (frob-doc-line line arglist-symbols) result)
             (incf lines-consumed))
            (t
             ;; end of itemization
             (loop-finish))))
    (if
     ;; a single-line table isn't.
     (> (count-if (lambda (line) (> (length line) 0)) result) 1)
     (values lines-consumed
             `("" "@table @code" ,@(reverse result) "@end table" ""))
     nil)))

(defun string-as-lines (string)
  (coerce (with-input-from-string (s string)
            (loop for line = (read-line s nil nil)
               while line collect line))
          'vector))

(defun frob-docstring (docstring symbol-arglist)
  "Try to guess as much formatting for a raw docstring as possible."
  ;; Per-line processing is not necessary now, but it will be when we
  ;; attempt itemize / table auto-detection in docstrings
  (with-output-to-string (result)
    (let ((arglist-symbols (all-symbols symbol-arglist))
          (doc-lines (string-as-lines (texinfoify docstring nil))))
      (loop for line-number from 0 below (length doc-lines)
           for line = (svref doc-lines line-number)
         do (cond
              ((maybe-itemize-offset line)
               (multiple-value-bind (lines-consumed itemized-lines)
                   (collect-maybe-itemized-section doc-lines line-number
                                                   arglist-symbols)
                 (cond (lines-consumed
                        (dolist (item-line itemized-lines)
                          (write-line item-line result))
                        (incf line-number (1- lines-consumed)))
                       (t (write-line (frob-doc-line line arglist-symbols)
                             result)))))
              ((maybe-table-offset line)
               (multiple-value-bind (lines-consumed itemized-lines)
                   (collect-maybe-table-section doc-lines line-number
                                                   arglist-symbols)
                 (cond (lines-consumed
                        (dolist (item-line itemized-lines)
                          (write-line item-line result))
                        (incf line-number (1- lines-consumed)))
                       (t (write-line (frob-doc-line line arglist-symbols)
                             result)))))
              (t (write-line (frob-doc-line line arglist-symbols) result)))))))

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
             (type (let ((class (find-class symbol nil)))
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
    (type (let ((class (find-class symbol nil)))
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
      (string (prin1 (texinfoify part nil) s))
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
     (format nil "~{~A~^ ~}" 
             (mapcar #'texinfoify-arglist-part (argument-list symbol))))))

(defun hidden-superclass-name-p (class-name superclass-name)
  (let ((super-package (symbol-package superclass-name)))
    (or
     ;; KLUDGE: We assume that we don't want to advertise internal
     ;; classes in CP-lists, unless the symbol we're documenting is
     ;; internal as well.
     (and (member super-package #.'(mapcar #'find-package '(sb-pcl sb-int sb-kernel)))
	     (not (eq super-package (symbol-package class-name))))
     ;; KLUDGE: We don't generally want to advertise SIMPLE-ERROR or
     ;; SIMPLE-CONDITION in the CPLs of conditions that inherit them
     ;; simply as a matter of convenience. The assumption here is
     ;; that the inheritance is incidental unless the name of the
     ;; condition begins with SIMPLE-.
     (and (member superclass-name '(simple-error simple-condition))
	  (let ((prefix "SIMPLE-"))
	    (mismatch prefix (string class-name) :end2 (length prefix)))
	  t ; don't return number from MISMATCH
	  ))))

(defun hidden-slot-p (symbol slot)
  ;; FIXME: There is no pricipal reason to avoid the slot docs fo
  ;; structures and conditions, but their DOCUMENTATION T doesn't
  ;; currently work with them the way we'd like.
  (not (and (typep (find-class symbol nil) 'standard-class)
	    (documentation slot t))))

(defun classlike-p (symbol kind)
  (and (eq 'type kind)
       (let ((class (find-class symbol nil))) 
	 (some (lambda (type)
		 (typep class type))
	       '(structure-class standard-class sb-pcl::condition-class)))))

(defun def-body (symbol kind docstring)
  (with-output-to-string (s)
    (when (classlike-p symbol kind)
      (format s "Class precedence list: @code{~(~{@w{~A}~^, ~}~)}~%~%"
	      (remove-if (lambda (super)
			   (hidden-superclass-name-p symbol super))
			 (mapcar #'class-name
				 (sb-mop:class-precedence-list (find-class symbol)))))
      (let ((documented-slots (remove-if (lambda (slot)
					   (hidden-slot-p symbol slot))
					 (sb-mop:class-direct-slots (find-class symbol)))))
	(when documented-slots
	  (format s "Slots:~%@itemize~%")
	  (dolist (slot documented-slots)
	    (format s "@item ~(@code{~A} ~@[--- initargs: @code{~{@w{~S}~^, ~}}~]~)~%~%~A~%"
		    (sb-mop:slot-definition-name slot)
		    (sb-mop:slot-definition-initargs slot)
		    (frob-docstring (documentation slot t) nil)))
	  (format s "@end itemize~%~%"))))
    (write-string (frob-docstring docstring (ignore-errors (argument-list symbol))) s)))

(defun def-end (symbol kind)
  (declare (ignore symbol))
  (ecase kind
    ((compiler-macro function method-combination setf) "@end deffn")
    ((package variable) "@end defvr")
    ((structure type) "@end deftp")))

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
           do (write-texinfo out package symbol kind docstring)))
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
	      (write-texinfo out package symbol kind docstring))))
    directory))

(defun write-texinfo (stream package symbol kind docstring)
  (format stream "~&@anchor{~A}~%~A ~A:~A~@[ ~A~]~%~A~&~A~%~A~%~%"
	  (unique-name symbol package kind)
	  (def-begin symbol kind)
	  (texinfoify (package-name package))
	  (texinfoify symbol)
	  (def-arglist symbol kind)
	  (def-index symbol kind)
	  (def-body symbol kind docstring)
	  (def-end symbol kind)))
