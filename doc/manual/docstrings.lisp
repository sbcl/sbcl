;;;; A docstring extractor for the sbcl manual.  Creates
;;;; @include-ready documentation from the docstrings of exported
;;;; symbols of specified packages.

;;;; This software is part of the SBCL software system. SBCL is in the
;;;; public domain and is provided with absolutely no warranty. See
;;;; the COPYING file for more information.
;;;;
;;;; Written by Rudi Schlatte <rudi@constantly.at>, mangled by
;;;; Nikodemus Siivola. Brought closer to Markdown by Gabor Melis.

;;;; This code can convert a strict subset of Markdown to Texinfo.
;;;; Supported:
;;;;
;;;; - Inline code: `set this with monospace`
;;;;
;;;; - Indented code blocks are indented with 4 extra spaces after a
;;;;   blank line:
;;;;
;;;;     Like this:
;;;;
;;;;         void main();
;;;;
;;;; - Fenced code blocks are indented at the normal level after a
;;;;   blank line:
;;;;
;;;;     ```
;;;;     void main();
;;;;     ```
;;;;
;;;;     Use fenced code blocks only when you have consecutive code
;;;;     blocks, which would be collapsed into a single code block
;;;;     when indented.
;;;;
;;;; - Itemized lists (like this one). List items can span multiple
;;;;   lines.
;;;;
;;;;     - Nested lists are indented 4 spaces. A blank line required
;;;;       before the first one.
;;;;
;;;; Codification and Downcasing
;;;; ---------------------------
;;;
;;;; Summary: Some text in docstrings is automatically codified (e.g.
;;;; FOO -> `FOO`) and most code is downcased.
;;;;
;;;; We approximate the semantics of PAX::@CODIFICATION with the
;;;; settings PAX:*DOCUMENT-UPPERCASE-IS-CODE* and
;;;; PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE* both true.
;;;;
;;;; - Fully-qualified all-uppercase string representatation of
;;;;   symbols are codified (SB-EXT:CAS, :XYZ).
;;;;
;;;; - All-uppercase SYMBOL-NAMEs accessible in the package that was
;;;;   in effect when the definition with the docstring was compiled
;;;;   are codified.
;;;;
;;;; - When at least 3 uppercase characters are followed by a
;;;;   lowercase character (e.g. SETFable), then the uppercase prefix
;;;;   is codified with the previous rules.
;;;;
;;;; Detecting the package is a heuristic endeavour. See
;;;; GUESS-PACKAGE-FROM-ARGLIST and PACKAGE-OVERRIDE.
;;;;
;;;; When there is no corresponding symbol, the Markdown backtick
;;;; syntax (`PRINT`) can be used to codify.
;;;;
;;;; When there are no lowercase nor #\" characters in inline code (as
;;;; opposed to code blocks), be it auto-codified or explicitly
;;;; backticked, it's downcased.
;;;;
;;;; When there is a corresponding symbol, but codification or
;;;; downcasing should not happen, use backslash escapes.
;;;;
;;;; Escaping (following PAX::@OVERVIEW-OF-ESCAPING):
;;;;
;;;;   PRINT     -> @code{print}    (Should be autolinked, unimplemented)
;;;;   \PRINT    -> @code{print}    (Prevent autolinking)
;;;;   \\PRINT   -> PRINT           (Prevent autolinking and codification)
;;;;   `PRINT`   -> @code{print}    (Should be autolinked, unimplemented)
;;;;   `\PRINT`  -> @code{print}    (Prevent autolinking)
;;;;   `\\PRINT` -> @code{PRINT}    (Prevent autolinking and downcasing)
;;;;
;;;; Note that in docstrings, the backslashes need to be doubled.

;;;; TODO
;;;; * Method documentation untested
;;;; * Method sorting, somehow
;;;; * Index for macros & constants?
;;;; * This is getting complicated enough that tests would be good
;;;; * Nesting (currently only nested itemizations work)
;;;; * doc -> internal form -> texinfo (so that non-texinfo format are also
;;;;   easily generated)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-introspect))

(defpackage :sb-texinfo
  (:use :cl :sb-mop)
  (:shadow #:documentation)
  (:export #:generate-includes #:document-package)
  (:documentation
   "Tools to generate TexInfo documentation from docstrings."))

(in-package :sb-texinfo)

;;;; various specials and parameters

(defvar *texinfo-output*)
(defvar *texinfo-variables*)
(defvar *documentation-package*)

(defparameter *undocumented-packages* '(sb-pcl sb-int sb-kernel sb-sys sb-c))

(defparameter *documentation-types*
  '(compiler-macro
    function
    method-combination
    setf
    ;;structure  ; also handled by `type'
    type
    variable)
  "A list of symbols accepted as second argument of `documentation'")

(defparameter *character-replacements*
  '((#\* . "star") (#\/ . "slash") (#\+ . "plus")
    (#\< . "lt") (#\> . "gt"))
  "Characters and their replacement names that `alphanumize' uses. If
the replacements contain any of the chars they're supposed to replace,
you deserve to lose.")

(defparameter *characters-to-drop* '(#\\ #\` #\')
  "Characters that should be removed by `alphanumize'.")

(defparameter *texinfo-escaped-chars* "@{}"
  "Characters that must be escaped with #\@ for Texinfo.")

(defparameter *itemize-start-characters* '(#\* #\-)
  "Characters that might start an itemization in docstrings when
  at the start of a line.")

(defparameter *symbol-characters* "ABCDEFGHIJKLMNOPQRSTUVWXYZ*:-+&#'"
  "List of characters that make up symbols in a docstring.")

(defparameter *symbol-delimiters* " ,.!?;()'")

(defparameter *ordered-documentation-kinds*
  '(package type structure condition class macro))

;;;; utilities

(defun flatten (list)
  (cond ((null list)
         nil)
        ((consp (car list))
         (nconc (flatten (car list)) (flatten (cdr list))))
        ((null (cdr list))
         (cons (car list) nil))
        (t
         (cons (car list) (flatten (cdr list))))))

(defun whitespacep (char)
  (find char #(#\tab #\space #\page #\newline)))

(defun setf-name-p (name)
  (or (symbolp name)
      (and (listp name) (= 2 (length name)) (eq (car name) 'setf))))

(defgeneric specializer-name (specializer))

(defmethod specializer-name ((specializer eql-specializer))
  (list 'eql (eql-specializer-object specializer)))

(defmethod specializer-name ((specializer class))
  (class-name specializer))

(defun ensure-class-precedence-list (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (class-precedence-list class))

(defun specialized-lambda-list (method)
  ;; courtesy of AMOP p. 61
  (let* ((specializers (method-specializers method))
         (lambda-list (method-lambda-list method))
         (n-required (length specializers)))
    (append (mapcar (lambda (arg specializer)
                      (if  (eq specializer (find-class 't))
                           arg
                           `(,arg ,(specializer-name specializer))))
                    (subseq lambda-list 0 n-required)
                    specializers)
           (subseq lambda-list n-required))))

(defun string-lines (string)
  "Lines in STRING as a vector."
  (coerce (with-input-from-string (s string)
            (loop for line = (read-line s nil nil)
               while line collect line))
          'vector))

(defun indentation (line)
  "Position of first non-SPACE character in LINE."
  (position-if-not (lambda (c) (char= c #\Space)) line))

(defun docstring (x doc-type)
  (cl:documentation x doc-type))

(defun flatten-to-string (list)
  (format nil "~{~A~^-~}" (flatten list)))

(defun alphanumize (original)
  "Construct a string without characters like *`' that will f-star-ck
up filename handling. See `*character-replacements*' and
`*characters-to-drop*' for customization."
  (let ((name (remove-if (lambda (x) (member x *characters-to-drop*))
                         (if (listp original)
                             (flatten-to-string original)
                             (string original))))
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

;;;; generating various names

(defgeneric name (thing)
  (:documentation "Name for a documented thing. Names are either
symbols or lists of symbols."))

(defmethod name ((symbol symbol))
  symbol)

(defmethod name ((cons cons))
  cons)

(defmethod name ((package package))
  (package-name package))

(defmethod name ((method method))
  (list
   (generic-function-name (method-generic-function method))
   (method-qualifiers method)
   (specialized-lambda-list method)))

;;; Node names for DOCUMENTATION instances

(defgeneric name-using-kind/name (kind name doc))

(defmethod name-using-kind/name (kind (name string) doc)
  (declare (ignore kind doc))
  name)

(defmethod name-using-kind/name (kind (name symbol) doc)
  (declare (ignore kind))
  (format nil "~A ~A" (package-name (get-package doc)) name))

(defmethod name-using-kind/name (kind (name list) doc)
  (declare (ignore kind))
  (assert (setf-name-p name))
  (format nil "(setf ~A ~A)" (package-name (get-package doc)) (second name)))

(defmethod name-using-kind/name ((kind (eql 'method)) name doc)
  (flet ((specializers (ll)
           (let (result)
             (dolist (arg ll)
               (cond
                 ((member arg lambda-list-keywords) (return))
                 ((atom arg) (push t result))
                 (t (push (second arg) result))))
             (nreverse result))))
    (format nil "~A~{ ~A~} ~A"
            (name-using-kind/name nil (first name) doc)
            (second name)
            (specializers (third name)))))

(defun node-name (doc)
  "Returns TexInfo node name as a string for a DOCUMENTATION instance."
  (let ((kind (get-kind doc)))
    (format nil "~:(~A~) ~(~A~)" kind (name-using-kind/name kind (get-name doc) doc))))

(defun package-shortest-name (package)
  (let* ((names (cons (package-name package) (package-nicknames package)))
         (sorted (sort (copy-list names) #'< :key #'length)))
    (car sorted)))

(defun package-macro-name (package)
  (let ((short-name (package-shortest-name package)))
    (remove-if-not #'alpha-char-p (string-downcase short-name))))

;;; Definition titles for DOCUMENTATION instances

(defgeneric title-using-kind/name (kind name doc))

(defmethod title-using-kind/name (kind (name string) doc)
  (declare (ignore kind doc))
  name)

(defmethod title-using-kind/name (kind (name symbol) doc)
  (declare (ignore kind))
  (let* ((symbol-name (symbol-name name))
         (earmuffsp (and (char= (char symbol-name 0) #\*)
                         (char= (char symbol-name (1- (length symbol-name))) #\*)
                         (some #'alpha-char-p symbol-name))))
    (if earmuffsp
        (format nil "@~A{@earmuffs{~A}}" (package-macro-name (get-package doc)) (subseq symbol-name 1 (1- (length symbol-name))))
        (format nil "@~A{~A}" (package-macro-name (get-package doc)) name))))

(defmethod title-using-kind/name (kind (name list) doc)
  (declare (ignore kind))
  (assert (setf-name-p name))
  (format nil "@setf{@~A{~A}}" (package-macro-name (get-package doc)) (second name)))

(defmethod title-using-kind/name ((kind (eql 'method)) name doc)
  (format nil "~{~A ~}~A"
          (second name)
          (title-using-kind/name nil (first name) doc)))

(defun title-name (doc)
  "Returns a string to be used as name of the definition."
  (string-downcase (title-using-kind/name (get-kind doc) (get-name doc) doc)))

(defun include-pathname (doc)
  (let* ((kind (get-kind doc))
         (name (nstring-downcase
                (if (eq 'package kind)
                    (format nil "package-~A" (alphanumize (get-name doc)))
                    (format nil "~A-~A-~A"
                            (case (get-kind doc)
                              ((function generic-function) "fun")
                              (structure "struct")
                              (variable "var")
                              (otherwise (symbol-name (get-kind doc))))
                            (alphanumize (package-name (get-package doc)))
                            (alphanumize (get-name doc)))))))
    (make-pathname :name name  :type "texinfo")))

;;;; documentation class and related methods

(defclass documentation ()
  ((name :initarg :name :reader get-name)
   (kind :initarg :kind :reader get-kind)
   (string :initarg :string :reader get-string)
   (children :initarg :children :initform nil :reader get-children)
   (package :initform *documentation-package* :reader get-package)))

(defmethod print-object ((documentation documentation) stream)
  (print-unreadable-object (documentation stream :type t)
    (princ (list (get-kind documentation) (get-name documentation)) stream)))

(defgeneric make-documentation (x doc-type string))

(defmethod make-documentation ((x package) doc-type string)
  (declare (ignore doc-type))
  (make-instance 'documentation
                 :name (name x)
                 :kind 'package
                 :string string))

(defmethod make-documentation (x (doc-type (eql 'function)) string)
  (declare (ignore doc-type))
  (let* ((fdef (and (fboundp x) (fdefinition x)))
         (name x)
         (kind (cond ((and (symbolp x) (special-operator-p x))
                      'special-operator)
                     ((and (symbolp x) (macro-function x))
                      'macro)
                     ((typep fdef 'generic-function)
                      (assert (or (symbolp name) (setf-name-p name)))
                      'generic-function)
                     (fdef
                      (assert (or (symbolp name) (setf-name-p name)))
                      'function)))
         (children (when (eq kind 'generic-function)
                     (collect-gf-documentation fdef))))
    (make-instance 'documentation
                   :name (name x)
                   :string string
                   :kind kind
                   :children children)))

(defmethod make-documentation ((x method) doc-type string)
  (declare (ignore doc-type))
  (make-instance 'documentation
                 :name (name x)
                 :kind 'method
                 :string string))

(defmethod make-documentation (x (doc-type (eql 'type)) string)
  (make-instance 'documentation
                 :name (name x)
                 :string string
                 :kind (etypecase (find-class x nil)
                         (structure-class 'structure)
                         (standard-class 'class)
                         (sb-pcl::condition-class 'condition)
                         ((or built-in-class null) 'type))))

(defmethod make-documentation (x (doc-type (eql 'variable)) string)
  (make-instance 'documentation
                 :name (name x)
                 :string string
                 :kind (if (constantp x)
                           'constant
                           'variable)))

(defmethod make-documentation (x (doc-type (eql 'setf)) string)
  (declare (ignore doc-type))
  (make-instance 'documentation
                 :name (name x)
                 :kind 'setf-expander
                 :string string))

(defmethod make-documentation (x doc-type string)
  (make-instance 'documentation
                 :name (name x)
                 :kind doc-type
                 :string string))

(defun maybe-documentation (x doc-type)
  "Returns a DOCUMENTATION instance for X and DOC-TYPE, or NIL if
there is no corresponding docstring."
  (let ((docstring (docstring x doc-type)))
    (when docstring
      (make-documentation x doc-type docstring))))

(defun lambda-list (doc)
  (case (get-kind doc)
    ((package constant variable type structure class condition nil)
     nil)
    (method
     (third (get-name doc)))
    (t
     ;; KLUDGE: Eugh.
     ;;
     ;; believe it or not, the above comment was written before CSR
     ;; came along and obfuscated this.  (2005-07-04)
     (when (symbolp (get-name doc))
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
         (multiple-value-bind (ll unknown) (sb-introspect:function-lambda-list (get-name doc))
           (if unknown
               (values nil t)
               (clean ll))))))))

(defun get-string-name (x)
  (let ((name (get-name x)))
    (cond ((symbolp name)
           (symbol-name name))
          ((and (consp name) (eq 'setf (car name)))
           (symbol-name (second name)))
          ((stringp name)
           name)
          (t
           (error "Don't know which symbol to use for name ~S" name)))))

(defun documentation< (x y)
  (let ((p1 (position (get-kind x) *ordered-documentation-kinds*))
        (p2 (position (get-kind y) *ordered-documentation-kinds*)))
    (if (or (not (and p1 p2)) (= p1 p2))
        (string< (get-string-name x) (get-string-name y))
        (< p1 p2))))

;;;; turning text into texinfo

(defun escape-for-texinfo (string &optional downcasep)
  "Return STRING with characters in *TEXINFO-ESCAPED-CHARS* escaped
with #\@. Optionally downcase the result."
  (let ((result (with-output-to-string (s)
                  (loop for char across string
                        when (find char *texinfo-escaped-chars*)
                        do (write-char #\@ s)
                        do (write-char char s)))))
    (if downcasep (nstring-downcase result) result)))

(defun empty-p (line-number lines)
  (and (< -1 line-number (length lines))
       (not (indentation (svref lines line-number)))))


;;;; Codification

;;; These wouldn't be necessary if we implemented PAX::@CODIFIABLE and
;;; PAX::@INTERESTING properly.
(defvar *not-code* '("A" "I"))

;;; GUESS-PACKAGE-FROM-ARGLIST doesn't always guess right.
(defvar *docstring-packages*
  '(("SB-CONCURRENCY:GATEP" "SB-CONCURRENCY")
    ("SB-CONCURRENCY:MAILBOXP" "SB-CONCURRENCY")
    ("SB-CONCURRENCY:QUEUEP" "SB-CONCURRENCY")
    ("SB-EXT:INTERACTIVE-EVAL" "SB-IMPL")
    ("SB-EXT:PROCESS-P" "SB-IMPL")
    ("SB-EXT:PROCESS-STATUS-HOOK" "SB-IMPL")
    ("(SETF SB-EXT:READTABLE-NORMALIZATION)" "SB-IMPL")))

(defun package-override (name)
  (let ((fully-qualified-name (let ((*package* (find-package :cl)))
                                (prin1-to-string name))))
    (second (find fully-qualified-name *docstring-packages*
                  :key #'first :test #'equal))))

#+nil
(let ((*texinfo-output* *standard-output*)
      (*documentation-package* *package*))
  (write-texinfo-string "`XXXXX`")
  (write-texinfo-string "`\\XXXXX`")
  (write-texinfo-string "`\\\\XXXXX`")
  (write-texinfo-string "`Not allcaps`")
  (write-texinfo-string "- a
  c

x
")
  (write-texinfo-string "`(X Y*)"))

(defun interesting-name-p (name)
  (let ((name (if (and (plusp (length name))
                       (find (aref name 0) "'`"))
                  (subseq name 1)
                  name)))
    (or (find-package name)
        (if (and (plusp (length name))
                 (char= (aref name 0) #\:))
            (internedp (subseq name 1) :keyword)
            (let ((pos (position #\: name)))
              (if pos
                  (let ((package-name (subseq name 0 pos))
                        (symbol-name (subseq name (1+ pos))))
                    (when (and (plusp (length symbol-name))
                               (char= (aref symbol-name 0) #\:))
                      (setq symbol-name (subseq symbol-name 1)))
                    (if (and package-name (find-package package-name))
                        (internedp symbol-name package-name)
                        (internedp symbol-name *documentation-package*)))
                  (internedp name *documentation-package*)))))))

(defun internedp (symbol-name package)
  (nth-value 1 (find-symbol symbol-name package)))

(defun locate-symbols (line)
  "Return a list of index pairs of symbol-like parts of LINE."
  ;; This would be a good application for a regex ...
  (let (result)
    (flet ((grab (start end)
             (let ((name (subseq line start end)))
               (when (and (not (member name *not-code* :test #'equal))
                          (interesting-name-p name))
                 (push (list start end) result))))
           (got-symbol-p (start)
             (let ((end (when (< start (length line))
                          (position #\space line :start start))))
               (when end
                 (every (lambda (char) (find char *symbol-characters*))
                        (subseq line start end))))))
      (do ((begin nil)
           (maybe-begin t)
           (i 0 (1+ i)))
          ((>= i (length line))
           ;; symbol at end of line
           (when begin
             (grab begin i))
           (nreverse result))
        (cond
          ((and begin
                (or (find (char line i) *symbol-delimiters*)
                    ;; This catches lowercase suffixes. SETFable,
                    ;; PRINTs, CLASSes.
                    (and (<= (+ begin 3) i)
                         (lower-case-p (char line i)))
                    ;; For e.g. "T:"
                    (and (char= (char line i) #\:)
                         (or (= (1+ i) (length line))
                             (whitespacep (char line (1+ i)))))))
           ;; symbol end
           (grab begin i)
           (setf begin nil
                 maybe-begin t))
          ((and begin (not (find (char line i) *symbol-characters*)))
           ;; Not a symbol: abort
           (setf begin nil))
          ((and maybe-begin (not begin)
                (find (char line i) *symbol-characters*))
           ;; potential symbol begin at this position
           (setf begin i
                 maybe-begin nil))
          ((find (char line i) *symbol-delimiters*)
           ;; potential symbol begin after this position
           (setf maybe-begin t))
          ((and (eql #\( (char line i)) (got-symbol-p (1+ i)))
           ;; a type designator, or a function call as part of the text?
           (multiple-value-bind (exp end)
               (let ((*package* (find-package :cl-user)))
                 (ignore-errors (read-from-string line nil nil :start i)))
             (when exp
               (grab i end)
               (setf begin nil
                     maybe-begin nil
                     i end))))
          (t
           ;; Not reading a symbol, not at potential start of symbol
           (setf maybe-begin nil)))))))

(defun texinfo-line (line)
  "Format symbols in LINE texinfo-style: either as code or as
variables if the symbol in question is contained in symbols
*TEXINFO-VARIABLES*."
  (with-output-to-string (result)
    (let ((last 0))
      (dolist (symbol/index (locate-symbols line))
        (write-string (subseq line last (first symbol/index)) result)
        (let ((symbol-name (apply #'subseq line symbol/index)))
          (format result (if (member symbol-name *texinfo-variables*
                                     :test #'string=)
                             ;; FIXME: We don't use @var{} elsewhere.
                             ;; Should we here?
                             "@var{~A}"
                             "@code{~A}")
                  (string-downcase symbol-name)))
        (setf last (second symbol/index)))
      (write-string (subseq line last) result))))


;;;; SBCL-flavoured Markdown to Texinfo Parser
;;;; Replaces heuristic codification with strict Markdown rules.

(defun blankp (line)
  "Returns T if the line is empty or contains only whitespace."
  (null (indentation line)))

(defun process-inline-markdown (string)
  "Translates escapes (\*) and backticks (`FOO` -> @code{FOO}), while
delegating normal text to the existing TEXINFO-LINE heuristic
codifier."
  (let ((len (length string))
        (i 0)
        (raw-buffer (make-string-output-stream))
        (out (make-string-output-stream)))
    (flet ((flush-raw ()
             (let ((raw (get-output-stream-string raw-buffer)))
               (when (plusp (length raw))
                 (write-string (texinfo-line raw) out)))))
      (loop while (< i len)
            for char = (char string i)
            do (cond
                 ;; Escapes: \FOO
                 ((char= char #\\)
                  (flush-raw)
                  (incf i) ; Skip the backslash
                  (when (< i len)
                    (write-char (char string i) out)
                    (incf i)
                    ;; Protect the rest of the contiguous word from TEXINFO-LINE
                    (loop
                      while (and (< i len)
                                 (not (member (char string i)
                                              '(#\Space #\Tab #\Newline
                                                #\( #\) #\[ #\] #\{ #\}
                                                #\' #\" #\, #\. #\; #\? #\!))))
                      do (write-char (char string i) out)
                         (incf i))
                    (decf i)))
                 ;; Backticks: `CODE` with PAX downcasing and escape rules
                 ((char= char #\`)
                  (flush-raw)
                  (incf i)
                  (let ((code-buffer (make-string-output-stream)))
                    (loop while (and (< i len) (char/= (char string i) #\`))
                          do (write-char (char string i) code-buffer)
                             (incf i))
                    (let* ((code-str (get-output-stream-string code-buffer))
                           (slash-count (loop for c across code-str
                                              while (char= c #\\)
                                              count t))
                           ;; Consume up to 2 leading backslashes as PAX escapes
                           (actual-code (subseq code-str (min slash-count 2))))
                      (write-string "@code{" out)
                      (if (< slash-count 2)
                          ;; 0 or 1 backslash: Downcase if there are
                          ;; no lowercase letters (1 backslash turns
                          ;; off autolinking, which is naturally
                          ;; handled by bypassing TEXINFO-LINE).
                          (if (and (not (find-if #'lower-case-p actual-code))
                                   (not (find #\" actual-code)))
                              (write-string (string-downcase actual-code) out)
                              (write-string actual-code out))
                          ;; 2 backslashes turn off autolinking AND downcasing.
                          (write-string actual-code out))
                      (write-string "}" out))))
                 (t
                  (write-char char raw-buffer)))
               (incf i))
      (flush-raw)
      (get-output-stream-string out))))

(defun collect-fenced-code (lines starting-line base-indent)
  "Collects lines enclosed in ``` fences.
Returns (VALUES CONSUMED-COUNT TEXINFO-LINES)."
  (let* ((first-line (svref lines starting-line))
         (trimmed (string-left-trim " " first-line)))
    (when (and (>= (length trimmed) 3)
               (string= (subseq trimmed 0 3) "```"))
      (let ((lang (string-trim " " (subseq trimmed 3)))
            (consumed 1)
            (result nil))
        (loop for index from (1+ starting-line) below (length lines)
              for line = (svref lines index)
              for line-trimmed = (string-left-trim " " line)
              do (incf consumed)
              if (and (>= (length line-trimmed) 3)
                      (string= (subseq line-trimmed 0 3) "```"))
                do (loop-finish) ; Closing fence found
              else
                ;; Strip up to the base indentation of the environment
                do (push (if (and (indentation line) (>= (indentation line) base-indent))
                             (subseq line base-indent)
                             line)
                         result))
        (let ((env (if (string-equal lang "lisp") "lisp" "example")))
          (values consumed
                  `(,(format nil "@~A" env)
                    ,@(nreverse result)
                    ,(format nil "@end ~A" env))))))))

(defun collect-indented-code (lines starting-line base-indent)
  "Collects lines using the classic 4-space indentation rule."
  ;; An indented code block must be by a blank line (or be the first line).
  (unless (and (> starting-line 0)
               (not (blankp (svref lines (1- starting-line)))))
    (let ((indent (indentation (svref lines starting-line))))
      (when (and indent (>= indent (+ base-indent 4)))
        (let ((consumed 0)
              (result nil))
          (loop for index from starting-line below (length lines)
                for line = (svref lines index)
                for line-indent = (indentation line)
                do (cond
                     ((blankp line)
                      ;; Blank lines are allowed inside indented code blocks
                      (push "" result)
                      (incf consumed))
                     ((>= line-indent (+ base-indent 4))
                      (push (subseq line (+ base-indent 4)) result)
                      (incf consumed))
                     (t
                      (loop-finish)))) ; Indentation dropped, code block ends
          ;; Trim trailing empty lines
          (loop while (and result (string= (car result) ""))
                do (pop result) (decf consumed))
          (if result
              (values consumed `("@example" ,@(nreverse result) "@end example"))
              nil))))))

(defun maybe-itemize-offset (line)
  "Returns the indent if the line starts with a Markdown list marker (- or *)."
  (let ((indent (indentation line)))
    (when indent
      (let ((trimmed (string-left-trim " " line)))
        (when (and (>= (length trimmed) 2)
                   (member (char trimmed 0) '(#\- #\*))
                   (char= (char trimmed 1) #\Space))
          indent)))))

(defun collect-markdown-itemize (lines starting-line base-indent)
  "Collects a list, strictly enforcing the 4-space rule for list bodies."
  (let ((this-offset (maybe-itemize-offset (svref lines starting-line))))
    (when (and this-offset (= this-offset base-indent))
      (let ((result nil)
            (lines-consumed 0)
            (child-base (+ base-indent 4)))
        (loop for line-number = starting-line then (+ starting-line
                                                      lines-consumed)
              while (< line-number (length lines))
              for line = (svref lines line-number)
              for indent = (indentation line)
              for offset = (maybe-itemize-offset line)
              do (cond
                   ((blankp line)
                    ;; Blank lines inside lists are buffered
                    (push "" result)
                    (incf lines-consumed))
                   ;; New Item in the same list
                   ((and offset (= offset base-indent))
                    (push (format nil "@item ~A"
                                  (process-inline-markdown
                                   (subseq line (+ offset 2))))
                          result)
                    (incf lines-consumed))
                   ;; Indented block/text inside the list item (>= 4 spaces)
                   ((and indent (>= indent child-base))
                    (multiple-value-bind (sub-consumed sub-result)
                        (parse-markdown-blocks lines line-number child-base)
                      (if sub-consumed
                          (progn
                            (setf result (append (reverse sub-result) result))
                            (incf lines-consumed sub-consumed))
                          ;; Fallback: normal text continuing the item body
                          (progn
                            (push (process-inline-markdown
                                   (subseq line child-base)) result)
                            (incf lines-consumed)))))
                   ;; Normal text continuing the item body (indent >
                   ;; base-indent, but < child-base)
                   ((and indent (> indent base-indent))
                    (push (process-inline-markdown line) result)
                    (incf lines-consumed))
                   ;; If we get here, the line is NOT a new bullet,
                   ;; and it less than 4 spaces of relative
                   ;; indentation, so the list is over.
                   (t
                    (loop-finish))))
        ;; Trim trailing empty lines so they return to the outer scope.
        (loop while (and result (string= (car result) ""))
              do (pop result) (decf lines-consumed))

        (values lines-consumed `("@itemize" ,@(reverse result)
                                 "@end itemize"))))))

(defun parse-markdown-blocks (lines index base-indent)
  "Parse the line at INDEX as a Markdown block.
Return (VALUES CONSUMED RESULT)."
  (let ((line (svref lines index)))
    (multiple-value-bind (n-lines-consumed result)
        (collect-fenced-code lines index base-indent)
      (cond
        (n-lines-consumed
         (values n-lines-consumed result))
        ((maybe-itemize-offset line)
         (collect-markdown-itemize lines index (maybe-itemize-offset line)))
        ((and (indentation line) (>= (indentation line) (+ base-indent 4)))
         (collect-indented-code lines index base-indent))
        (t nil)))))

(defmacro with-markdown-section (index &rest forms)
  `(multiple-value-bind (count collected) (progn ,@forms)
     (when count
       (dolist (line collected)
         (write-line line *texinfo-output*))
       (incf ,index count)
       t)))

(defun write-texinfo-string (string &optional lambda-list)
  (let ((*texinfo-variables* (flatten lambda-list))
        ;; Note: The heuristic upcaser (e.g., FOO to @code{foo}) can either run on 'string'
        ;; before escape-for-texinfo, or be integrated into process-inline-markdown.
        (lines (string-lines (escape-for-texinfo string nil)))
        (line-number 0))
    (loop while (< line-number (length lines))
          for line = (svref lines line-number)
          do (unless (with-markdown-section line-number
                       (parse-markdown-blocks lines line-number 0))
               ;; If it wasn't a block, process it as a normal inline string
               (write-line (process-inline-markdown line) *texinfo-output*)
               (incf line-number)))))


;;;; texinfo formatting tools

(defun hide-superclass-p (class-name super-name)
  (let ((super-package (symbol-package super-name)))
    (or
     ;; KLUDGE: We assume that we don't want to advertise internal
     ;; classes in CP-lists, unless the symbol we're documenting is
     ;; internal as well.
     (and (member super-package #.'(mapcar #'find-package *undocumented-packages*))
          (not (eq super-package (symbol-package class-name))))
     ;; KLUDGE: We don't generally want to advertise SIMPLE-ERROR or
     ;; SIMPLE-CONDITION in the CPLs of conditions that inherit them
     ;; simply as a matter of convenience. The assumption here is that
     ;; the inheritance is incidental unless the name of the condition
     ;; begins with SIMPLE-.
     (and (member super-name '(simple-error simple-condition))
          (let ((prefix "SIMPLE-"))
            (mismatch prefix (string class-name) :end2 (length prefix)))
          t ; don't return number from MISMATCH
          ))))

(defun hide-slot-p (symbol slot)
  ;; FIXME: There is no pricipal reason to avoid the slot docs fo
  ;; structures and conditions, but their DOCUMENTATION T doesn't
  ;; currently work with them the way we'd like.
  (not (and (typep (find-class symbol nil) 'standard-class)
            (docstring slot t))))

(defun texinfo-anchor (doc &aux *print-pretty*)
  (format *texinfo-output* "@anchor{~A}~%" (node-name doc)))

;;; KLUDGE: &AUX *PRINT-PRETTY* here means "no linebreaks please"
(defun texinfo-begin (doc &aux *print-pretty*)
  (let ((kind (get-kind doc)))
    (format *texinfo-output* "@~A {~:(~A~)} ~(~A~)"
            (case kind
              ((package constant variable)
               "defvr")
              ((structure class condition type)
               "deftp")
              (t
               "deffn"))
            (map 'string (lambda (char) (if (eql char #\-) #\Space char)) (string kind))
            (title-name doc))
    (multiple-value-bind (lambda-list unknown) (lambda-list doc)
      (cond (unknown
             (format *texinfo-output* " @emph{lambda list not known}"))
            ((not lambda-list))
            (t
             ;; &foo would be amusingly bold in the pdf thanks to
             ;; TeX/Texinfo interactions,so we escape the ampersand --
             ;; amusingly for TeX.  sbcl.texinfo defines macros that
             ;; expand @andkey and friends to &key.
             (format *texinfo-output* " ~(~{~A~^ ~}~)"
                     (mapcar (lambda (name)
                               (if (member name lambda-list-keywords)
                                   (format nil "@and~A{}"
                                           (remove #\- (subseq (string name) 1)))
                                   name))
                             lambda-list)))))
    (format *texinfo-output* "~%")))

(defun texinfo-inferred-body (doc)
  (when (member (get-kind doc) '(class structure condition))
    (let ((name (get-name doc)))
      ;; class precedence list
      (format *texinfo-output* "@raggedright~%Class precedence list: ~(~{@code{@w{~A}}~^, ~}~)~%@end raggedright~%~%"
              (remove-if (lambda (class)  (hide-superclass-p name class))
                         (mapcar #'class-name (ensure-class-precedence-list (find-class name)))))
      ;; slots
      (let ((slots (remove-if (lambda (slot) (hide-slot-p name slot))
                              (class-direct-slots (find-class name)))))
        (when slots
          (format *texinfo-output* "Slots:~%@itemize~%")
          (dolist (slot slots)
            (format *texinfo-output*
                    "@item ~(@code{~A}~#[~:; --- ~]~
                      ~:{~2*~@[~2:*~A~P: ~{@code{@w{~S}}~^, ~}~]~:^; ~}~)~%~%"
                    (slot-definition-name slot)
                    (remove
                     nil
                     (mapcar
                      (lambda (name things)
                        (if things
                            (list name (length things) things)))
                      '("initarg" "reader"  "writer")
                      (list
                       (slot-definition-initargs slot)
                       (slot-definition-readers slot)
                       (slot-definition-writers slot)))))
            ;; FIXME: Would be neater to handler as children
            (write-texinfo-string (docstring slot t)))
          (format *texinfo-output* "@end itemize~%~%"))))))

(defun texinfo-body (doc)
  (write-texinfo-string (sanitize-docstring (get-string doc))))

(defun texinfo-end (doc)
  (write-line (case (get-kind doc)
                ((package variable constant) "@end defvr")
                ((structure type class condition) "@end deftp")
                (t "@end deffn"))
              *texinfo-output*))

(defun write-texinfo (doc)
  "Writes TexInfo for a DOCUMENTATION instance to *TEXINFO-OUTPUT*."
  (let ((*documentation-package*
          (or (package-override (get-name doc))
              (guess-package-from-arglist (lambda-list doc))
              (let ((p (get-package doc)))
                (cond ((eq p (find-package :cl))
                       ;; Most of the implementation of CL is done under
                       ;; (IN-PACKAGE :SB-IMPL).
                       (find-package :sb-impl))
                      ((eq p (find-package :sequence))
                       (find-package :sb-impl))
                      (t
                       (get-package doc)))))))
    (texinfo-anchor doc)
    (texinfo-begin doc)
    (texinfo-inferred-body doc)
    (texinfo-body doc)
    (texinfo-end doc)
    ;; FIXME: Children should be sorted one way or another
    (mapc #'write-texinfo (get-children doc))))


;;;; Utilities lifted from MGL-PAX

(defun sanitize-docstring (docstring)
  (let ((indentation (docstring-indentation docstring)))
    (strip-docstring-indent docstring indentation t)))

;;; Return the minimum number of leading spaces in non-blank lines
;;; after the first.
(defun docstring-indentation (docstring &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (and (or (not first-line-special-p) (plusp i))
                          (not (blankp line)))
                 (when (or (null n-min-indentation)
                           (< (n-leading-spaces line) n-min-indentation))
                   (setq n-min-indentation (n-leading-spaces line))))))
    (or n-min-indentation 0)))

(defun n-leading-spaces (line)
  (let ((n 0))
    (loop for i below (length line)
          while (char= (aref line i) #\Space)
          do (incf n))
    n))

(defun subseq* (seq start)
  (subseq seq (min (length seq) start)))

(defun strip-docstring-indent (docstring indentation first-line-special-p)
  (with-output-to-string (out)
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            do (multiple-value-bind (line missing-newline-p)
                   (read-line s nil nil)
                 (unless line
                   (return))
                 (write-string (if (and first-line-special-p
                                        (zerop i))
                                   line
                                   (subseq* line indentation))
                               out)
                 (unless missing-newline-p
                   (terpri out)))))))

;;; Unexported argument names are highly informative about *PACKAGE*
;;; at read time. No one ever uses fully-qualified internal symbols
;;; from another package for arguments, right?
(defun guess-package-from-arglist (args)
  (dolist (arg args)
    (when (and (symbolp arg)
               (not (external-symbol-in-any-package-p arg)))
      (return (symbol-package arg)))
    (when (and (listp arg)
               (symbolp (first arg))
               (not (external-symbol-in-any-package-p (first arg))))
      (return (symbol-package (first arg))))))

(defun external-symbol-in-any-package-p (symbol)
  (loop for package in (list-all-packages)
          thereis (external-symbol-p symbol package)))

(defun external-symbol-p (symbol &optional (package (symbol-package symbol)))
  (and package
       (multiple-value-bind (symbol* status)
           (find-symbol (symbol-name symbol) package)
         (and (eq status :external)
              (eq symbol symbol*)))))


;;;; main logic

(defun collect-gf-documentation (gf)
  "Collects method documentation for the generic function GF"
  (loop for method in (generic-function-methods gf)
        for doc = (maybe-documentation method t)
        when doc
        collect doc))

(defun collect-name-documentation (name)
  (loop for type in *documentation-types*
        for doc = (maybe-documentation name type)
        when doc
        collect doc))

(defun collect-symbol-documentation (symbol)
  "Collects all docs for a SYMBOL and (SETF SYMBOL), returns a list of
the form DOC instances. See `*documentation-types*' for the possible
values of doc-type."
  (nconc (collect-name-documentation symbol)
         (collect-name-documentation (list 'setf symbol))))

(defun collect-documentation (package)
  "Collects all documentation for all external symbols of the given
package, as well as for the package itself."
  (let* ((*documentation-package* (find-package package))
         (docs nil))
    (check-type package package)
    (do-external-symbols (symbol package)
      (setf docs (nconc (collect-symbol-documentation symbol) docs)))
    (let ((doc (maybe-documentation *documentation-package* t)))
      (when doc
        (push doc docs)))
    docs))

(defmacro with-texinfo-file (pathname &body forms)
  `(with-open-file (*texinfo-output* ,pathname
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
    ,@forms))

(defun write-package-macro (package)
  (let* ((package-name (package-shortest-name package))
         (macro-name (package-macro-name package)))
    ;; KLUDGE: SB-SEQUENCE has a shorter nickname SEQUENCE, but we
    ;; want to document the SB- variant.
    (when (eql (find-package "SB-SEQUENCE") (find-package package))
      (setf package-name "SB-SEQUENCE"))
    (write-packageish-macro package-name macro-name)))

(defun write-packageish-macro (package-name macro-name)
  ;; a word of explanation about the iftex branch here is probably
  ;; warranted.  The package information should be present for
  ;; clarity, because these produce body text as well as index
  ;; entries (though in info output it's more important to use a
  ;; very restricted character set because the info reader parses
  ;; the link, and colon is a special character).  In TeX output we
  ;; make the package name unconditionally small, and arrange such
  ;; that the start of the symbol name is at a constant horizontal
  ;; offset, that offset being such that the longest package names
  ;; have the "sb-" extending into the left margin.  (At the moment,
  ;; the length of the longest package name, sb-concurrency, is
  ;; hard-coded).
  (format *texinfo-output* "~
@iftex
@macro ~A{name}
{@smallertt@phantom{concurrency:}~@[@llap{~(~A~):}~]}\\name\\
@end macro
@end iftex
@ifinfo
@macro ~2:*~A{name}
\\name\\
@end macro
@end ifinfo
@ifnottex
@ifnotinfo
@macro ~:*~A{name}
\\name\\ ~@[[~(~A~)]~]
@end macro
@end ifnotinfo
@end ifnottex~%"
          macro-name package-name))

(defun generate-includes (directory &rest packages)
  "Create files in `directory' containing Texinfo markup of all
docstrings of each exported symbol in `packages'. `directory' is
created if necessary. If you supply a namestring that doesn't end in a
slash, you lose. The generated files are of the form
\"<doc-type>_<packagename>_<symbol-name>.texinfo\" and can be included
via @include statements. Texinfo syntax-significant characters are
escaped in symbol names, but if a docstring contains invalid Texinfo
markup, you lose."
  (handler-bind ((warning #'muffle-warning))
    (let ((directory (merge-pathnames (pathname directory))))
      (ensure-directories-exist directory)
      (dolist (package packages)
        (dolist (doc (collect-documentation (find-package package)))
          (with-texinfo-file (merge-pathnames (include-pathname doc) directory)
            (write-texinfo doc))))
      (with-texinfo-file (merge-pathnames "package-macros.texinfo" directory)
        (dolist (package packages)
          (write-package-macro package))
        (write-packageish-macro nil "nopkg"))
      directory)))

(defun document-package (package &optional filename)
  "Create a file containing all available documentation for the
exported symbols of `package' in Texinfo format. If `filename' is not
supplied, a file \"<packagename>.texinfo\" is generated.

The definitions can be referenced using Texinfo statements like
@ref{<doc-type>_<packagename>_<symbol-name>.texinfo}. Texinfo
syntax-significant characters are escaped in symbol names, but if a
docstring contains invalid Texinfo markup, you lose."
  (handler-bind ((warning #'muffle-warning))
    (let* ((package (find-package package))
           (filename (or filename (make-pathname
                                   :name (string-downcase (package-name package))
                                   :type "texinfo")))
           (docs (sort (collect-documentation package) #'documentation<)))
      (with-texinfo-file filename
        (dolist (doc docs)
          (write-texinfo doc)))
      filename)))
