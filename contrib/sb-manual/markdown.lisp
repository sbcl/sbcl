;;;; A Markdown-to-Texinfo converter for the SBCL manual.

;;;; This software is part of the SBCL software system. SBCL is in the
;;;; public domain and is provided with absolutely no warranty. See
;;;; the COPYING file for more information.
;;;;
;;;; Written by Rudi Schlatte <rudi@constantly.at>, mangled by
;;;; Nikodemus Siivola. Brought closer to Markdown, extended with
;;;; section linking and concept indexing by Gabor Melis.

(in-package :sb-manual)

(defvar *concept-keys-to-prepend*)

;;; MARKDOWN-TO-TEXINFO converts a strict subset of Markdown to
;;; Texinfo. It also codifies (marks up as code) and downcases
;;; uppercase symbols (those that actually exist in the image), and
;;; autolinks references to sections, attempting to approximate PAX
;;; semantics.
;;;
;;; Note that for writing docstrings, you need to know two more pieces:
;;;
;;; - See REINDENT-DOCSTRING for how the docstring relates to the
;;;   Markdown string passed to MARKDOWN-TO-TEXINFO.
;;;
;;; - See DOCSTRING-PACKAGE to understand what *PACKAGE* is when
;;;   MARKDOWN-TO-TEXINFO is called. This is package in effect when
;;;   the docstring was READ. If it's wrong, you will see missed
;;;   opportunities for codification and linking.
;;;
;;;
;;; Markdown Formatting
;;; -------------------
;;;
;;; The supported Markdown constructs are:
;;;
;;; - Emphasis: _italic_ -> @emph{italic}
;;;
;;; - Strong emphasis: __bold__ -> @strong{bold}
;;;
;;; - Inline code: `monospace` -> @code{monospace}
;;;
;;; - Itemized lists (like this one). List items can span multiple
;;;   lines.
;;;
;;;     - Nested lists are indented 4 spaces. A blank line is required
;;;       before the first one.
;;;
;;; - Indented code blocks are indented with 4 extra spaces after a
;;;   blank line:
;;;
;;;     Like this:
;;;
;;;         void main();
;;;
;;; - Fenced code blocks are indented at the normal level after a
;;;   blank line:
;;;
;;;     ```
;;;     void main();
;;;     ```
;;;
;;;     Use fenced code blocks only when you have consecutive code
;;;     blocks, which would be collapsed into a single code block when
;;;     indented.
;;;
;;; - Blockquotes:
;;;
;;;     > _Note_: They can span multiple lines and anything can be
;;;     > nested in them. Rendered indented, typically with a vertical
;;;     > bar on the left.
;;;
;;; - Note that ``abc'' is *not* supported and *will* screw up the
;;;   rendering of the Markdown. This is because it's impossible to
;;;   reconcile it with backticks: consider the possible semantics of
;;;
;;;         ``x'' and ``y''
;;;
;;;
;;; Codification and Downcasing
;;; ---------------------------
;;
;;; Summary: Some text is automatically codified (e.g. FOO -> `FOO`)
;;; and most code is downcased.
;;;
;;; We approximate the semantics of PAX::@CODIFICATION with the
;;; settings PAX:*DOCUMENT-UPPERCASE-IS-CODE* and
;;; PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE* both true.
;;;
;;; - Fully-qualified all-uppercase string representatation of symbols
;;;   are codified (SB-EXT:CAS, :XYZ).
;;;
;;; - All-uppercase SYMBOL-NAMEs accessible in *PACKAGE*.
;;;
;;; - When at least 3 uppercase characters are followed by a lowercase
;;;   character (e.g. SETFable), then the uppercase prefix is codified
;;;   with the previous rules.
;;;
;;; When there is no corresponding symbol, the Markdown backtick
;;; syntax (`PRINT`) can be used to codify.
;;;
;;; When there are no lowercase nor #\" characters in inline code (as
;;; opposed to code blocks), be it auto-codified or explicitly
;;; backticked, it's downcased.
;;;
;;; When there is a corresponding symbol, but codification or
;;; downcasing should not happen, use backslash escapes.
;;;
;;; Escaping (following PAX::@OVERVIEW-OF-ESCAPING):
;;;
;;;   PRINT     -> @code{print}    (Should be autolinked, unimplemented)
;;;   \PRINT    -> @code{print}    (Prevent autolinking)
;;;   \\PRINT   -> PRINT           (Prevent autolinking and codification)
;;;   `PRINT`   -> @code{print}    (Should be autolinked, unimplemented)
;;;   `\PRINT`  -> @code{print}    (Prevent autolinking)
;;;   `\\PRINT` -> @code{PRINT}    (Prevent autolinking and downcasing)
;;;
;;; Note that in docstrings, the backslashes need to be doubled.
;;;
;;;
;;; Linking
;;; -------
;;;
;;; - <http...> -> @url{http...}
;;;
;;; - [label](uri) -> @uref{uri, label}
;;;
;;; - [label][id] -> label
;;;
;;;     This just strips Markdown reference links. These are used by
;;;     PAX to disambiguate, e.g. "[FUNCTION][type]" links to the
;;;     FUNCTION class only while FUNCTION links to both the class and
;;;     macro.
;;;
;;; - SECTION references (see DEFSECTION):
;;;
;;;     @SECTION-NAME -> @ref{<section name>}
;;;
;;; - CONCEPT references (see DEFINE-CONCEPT):
;;;
;;;     - pure concept: @CONCEPT-NAME -> "" (no output)
;;;     - titled concept: @CONCEPT-NAME -> <title>
;;;
;;; TODO:
;;;
;;; - Maybe implement glossary-terms (for books, "safe type", etc).
(defun markdown-to-texinfo (string &optional lambda-list)
  (let ((*texinfo-local-variables* (flatten lambda-list))
        (lines (string-lines string))
        (line-number 0)
        (current-paragraph nil))
    (declare (special *texinfo-local-variables*))
    (flet ((flush-paragraph ()
             (when current-paragraph
               (let* ((*concept-keys-to-prepend* ())
                      (string (process-inline-markdown
                               (format nil "~{~A~^~%~}"
                                       (nreverse current-paragraph)))))
                 (write-concept-keys *concept-keys-to-prepend* t)
                 (write-string string))
               (terpri)
               (setf current-paragraph nil))))
      (loop while (< line-number (length lines))
            for line = (svref lines line-number)
            do (let ((*concept-keys-to-prepend* ()))
                 (multiple-value-bind (count collected)
                     (parse-markdown-block lines line-number 0)
                   (cond
                     (count
                      (flush-paragraph)
                      (write-concept-keys *concept-keys-to-prepend* t)
                      (dolist (c collected)
                        (write-line c))
                      (incf line-number count))
                     ((blankp line)
                      (flush-paragraph)
                      (write-line line)
                      (incf line-number))
                     (t
                      (push line current-paragraph)
                      (incf line-number))))))
      (flush-paragraph))))


;;;; Utilities

(defun flatten (list)
  (cond ((null list)
         nil)
        ((consp (car list))
         (nconc (flatten (car list)) (flatten (cdr list))))
        ((null (cdr list))
         (cons (car list) nil))
        (t
         (cons (car list) (flatten (cdr list))))))

(defun flatten-to-string (list)
  (format nil "~{~A~^-~}" (flatten list)))

(defun internedp (symbol-name package)
  (nth-value 1 (find-symbol symbol-name package)))

(defun external-symbol-p (symbol &optional (package (symbol-package symbol)))
  (and package
       (multiple-value-bind (symbol* status)
           (find-symbol (symbol-name symbol) package)
         (and (eq status :external)
              (eq symbol symbol*)))))


;;;; Texinfo escaping

(defparameter *texinfo-special-chars* "@{}")

(defun escape-texinfo (string)
  (with-output-to-string (s)
    (loop for char across string
          do (when (find char *texinfo-special-chars*)
               (write-char #\@ s))
             (write-char char s))))

(defun unescape-texinfo (string)
  (with-output-to-string (s)
    (let ((prev-escape-p nil))
      (loop for char across string
            do (cond (prev-escape-p
                      (write-char char s)
                      (setq prev-escape-p nil))
                     ((char= char #\@)
                      (setq prev-escape-p t))
                     (t
                      (write-char char s)))))))

(progn
  (assert (equal (escape-texinfo "@code{x}") "@@code@{x@}"))
  (assert (equal (unescape-texinfo "@@code@{x@}") "@code{x}")))


;;;; Codification (following PAX::@CODIFICATION)

(defvar *lower-case-chars* "abcdefghijklmnopqrstuvwxyz")

(defun codifiable-bounds (word)
  (when (codifiable-word-p word)
    ;; PAX::@NAMES-IN-RAW-NAMES is involved. We only try two simple
    ;; cases to get a PAX::@NAME.
    (flet ((try-name (start end)
             (let ((name (subseq word start end)))
               (multiple-value-bind (symbol foundp)
                   (read-symbol-without-interning name)
                 (when (and foundp (interesting-name-p word symbol))
                   (return-from codifiable-bounds (values start end)))))))
      ;; 1. Trim the lower-case characters
      (let* ((name (string-left-trim *lower-case-chars* word))
             (name-start (- (length word) (length name)))
             (name (string-right-trim *lower-case-chars* name))
             (name-end (+ name-start (length name))))
        (try-name name-start name-end))
      ;; 2. Find the upper-case core
      (multiple-value-bind (name-start name-end) (uppercase-core-bounds word)
        (when name-start
          (try-name name-start name-end))))))

(defun codifiable-word-p (string)
  (uppercase-core-bounds string))

(defun read-symbol-without-interning (string)
  (if (and (plusp (length string))
           (char= (aref string 0) #\:))
      (find-symbol (subseq string 1) :keyword)
      (let ((pos (position #\: string)))
        (if pos
            (let* ((package-name (subseq string 0 pos))
                   (symbol-name (subseq string (1+ pos)))
                   (double-colon-p
                     (and (plusp (length symbol-name))
                          (char= (aref symbol-name 0) #\:))))
              (when double-colon-p
                (setq symbol-name (subseq symbol-name 1)))
              (if package-name
                  (when (find-package package-name)
                    (multiple-value-bind (symbol status)
                        (find-symbol symbol-name package-name)
                      (when (or double-colon-p
                                (eq status :external))
                        (values symbol status))))
                  (find-symbol symbol-name *package*)))
            (find-symbol string *package*)))))

;;; Approximating PAX::@INTERESTING. This is only called when we
;;; already found the interned SYMBOL.
(defun interesting-name-p (word symbol)
  (or (<= 3 (length word))
      (external-symbol-p symbol)
      (has-local-reference-p symbol)))

(defun uppercase-core-bounds (string)
  (let* ((first-uppercase-pos (position-if #'upper-case-p string))
         (last-uppercase-pos (position-if #'upper-case-p string
                                          :from-end t)))
    (when (and first-uppercase-pos
               (if (= last-uppercase-pos first-uppercase-pos)
                   (notany #'lower-case-p string)
                   (not (find-if #'lower-case-p string
                                 :start (1+ first-uppercase-pos)
                                 :end last-uppercase-pos))))
      (values first-uppercase-pos (1+ last-uppercase-pos)))))

(defvar *texinfo-local-variables* ())

(defun has-local-reference-p (name)
  (find name *texinfo-local-variables*))

#+nil
(progn
  (assert (equal (multiple-value-list (codifiable-bounds "PRINT"))
                 '(0 5)))
  (assert (equal (multiple-value-list (codifiable-bounds "T"))
                 '(0 1)))
  (if (internedp "A" *package*)
      (assert (equal (multiple-value-list (codifiable-bounds "A"))
                     '(0 1)))
      (assert (null (codifiable-bounds "A"))))
  (assert (equal (multiple-value-list (codifiable-bounds "*FEATURES*"))
                 '(0 10))))

;;; We parse words (e.g. nonREADable) and find symbols in them.
(defparameter *word-characters*
  (format nil "abcdefghijklmnopqrstuvwxyz~
               ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~
               *~~@:-+&=<>#'"))

(defparameter *word-delimiters* " ',.!?;()[]{}")

;;; Return a list of index pairs of symbol-like parts of LINE.
(defun locate-symbols (line)
  (let (result)
    (flet ((grab (word-start word-end)
             (let ((word (subseq line word-start word-end)))
               (multiple-value-bind (name-start name-end)
                   (codifiable-bounds word)
                 (when name-start
                   (push (list (+ word-start name-start)
                               (+ word-start name-end))
                         result)))))
           (got-symbol-p (start)
             (let ((end (when (< start (length line))
                          (position-if (lambda (c)
                                         (or (whitespacep c)
                                             (find c *word-delimiters*)))
                                       line :start start))))
               (when end
                 (every (lambda (char) (find char *word-characters*))
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
                (or (whitespacep (char line i))
                    (find (char line i) *word-delimiters*)
                    ;; For e.g. "T:"
                    (and (char= (char line i) #\:)
                         (or (= (1+ i) (length line))
                             (whitespacep (char line (1+ i)))))))
           ;; symbol end
           (grab begin i)
           (setf begin nil
                 maybe-begin t))
          ((and begin (not (find (char line i) *word-characters*)))
           ;; Not a symbol: abort
           (setf begin nil))
          ((and maybe-begin (not begin)
                (find (char line i) *word-characters*))
           ;; potential symbol begin at this position
           (setf begin i
                 maybe-begin nil))
          ((or (whitespacep (char line i))
               (find (char line i) *word-delimiters*))
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
                     i (1- end)))))
          (t
           ;; Not reading a symbol, not at potential start of symbol
           (setf maybe-begin nil)))))))

(unwind-protect
     (progn
       (defsection @test-section (:title "test section"))
       (defsection @test5 (:title "Test5"))
       (assert (equal (locate-symbols "PRINT") '((0 5))))
       (assert (equal (locate-symbols "CL:PRINT") '((0 8))))
       (assert (equal (locate-symbols "*FEATURES*") '((0 10))))
       (assert (equal (locate-symbols "SETFable") '((0 4))))
       (assert (equal (locate-symbols "SETF-able") '((0 4))))
       (assert (equal (locate-symbols "nonREADable") '((3 7))))
       (assert (equal (locate-symbols "NOSUCHSYMBOL-able") '()))
       (assert (equal (locate-symbols "ASDF-like") '()))
       (assert (equal (locate-symbols "@TEST-SECTION") '((0 13))))
       (assert (equal (locate-symbols "SB-MANUAL:@TEST-SECTION") '((0 23))))
       (assert (equal (locate-symbols "@NOSUCHSECTION") '()))
       (assert (equal (locate-symbols "@TEST5") '((0 6))))
       (assert (equal (locate-symbols ":IR1-CONVERT") '((0 12)))))
  (makunbound '@test-section)
  (makunbound '@test5))


;;;; Processing Markdown inline elements

;;; Format symbols either as Texinfo code or as variables if the
;;; symbol in question is contained in symbols *TEXINFO-LOCAL-VARIABLES*.
(defun codify-and-link (line)
  (with-output-to-string (result)
    (let ((last 0))
      (dolist (symbol/index (locate-symbols line))
        ;; Flush unwritten text since the end of the previous symbol.
        (write-string (escape-texinfo (subseq line last
                                              (first symbol/index)))
                      result)
        (let* ((symbol-name (apply #'subseq line symbol/index))
               (symbol (read-from-string symbol-name)))
          (cond ((doc-name-p symbol :section)
                 (format result "@ref{~A}"
                         (texinfo-node-id (symbol-value symbol))))
                ((doc-name-p symbol :concept)
                 (let* ((concept (symbol-value symbol))
                        (title (doctitle concept)))
                   (when title
                     (format result "~A" (escape-texinfo title)))
                   (setq *concept-keys-to-prepend*
                         (append *concept-keys-to-prepend*
                                 (multiplexing-concept-keys concept)))))
                (t
                 ;; We could use for @var{} if
                 ;; (HAS-LOCAL-REFERENCE-P SYMBOL).
                 (format result "@code{~A}"
                         (escape-texinfo (maybe-downcase symbol-name))))))
        (setf last (second symbol/index)))
      (write-string (escape-texinfo (subseq line last)) result))))

(defun write-concept-keys (keys stream)
  (dolist (key keys)
    (typecase key
      (list
       ;; We don't use @subentry because with it Texinfo always
       ;; presents it as as hierarchical list even if it has only one
       ;; branch.
       (format stream "~&@cindex~{ ~A~}~%" key))
      (symbol
       (assert (and (boundp key)
                    (typep (symbol-value key) (dummy 'concept)))
               () "Variable ~S does not hold a concept." key)
       (write-concept-keys (concept-keys (symbol-value key)) stream))
      (t
       (format stream "~&@cindex ~A~%" key)))))

(defvar *downcase-uppercase-code* t)

(defun maybe-downcase (string)
  (if *downcase-uppercase-code*
      (string-downcase string)
      string))

(defun texinfo-node-id (section)
  (let ((name (symbol-name (section-name section))))
    (assert (char= (char name 0) #\@))
    (let ((name (subseq name 1)))
      (assert (null (find-if (lambda (char)
                               (find char *texinfo-special-chars*))
                             name))
              () "Section name ~S contains special texinfo characters." name)
      (substitute #\Space #\- (string-downcase name)))))

(defun doc-name-p (symbol kind)
  (if *using-pax*
      (and (boundp symbol)
           (typep (symbol-value symbol) (dummy (ecase kind
                                                 (:section 'section)
                                                 (:concept 'concept)))))
      (lazy-doc-name-p symbol kind)))

(when (and (not *using-pax*)
           *downcase-uppercase-code*)
  (defsection @test-section (:title "Test Section"))
  (unwind-protect
       (progn
         (assert (equal (codify-and-link "@TEST-SECTION") "@ref{test section}"))
         (assert (equal (codify-and-link "@NOSUCHSECTION") "@@NOSUCHSECTION"))
         (assert (equal (codify-and-link ":START") "@code{:start}"))
         (assert (equal (codify-and-link "[:START") "[@code{:start}"))
         (assert (equal (codify-and-link "{:START") "@{@code{:start}")))
    (makunbound '@test-section)))

;;; Translate backticks, emphasis and codification escapes, while
;;; delegating normal text to CODIFY-AND-LINK.
(defun process-inline-markdown (string)
  (let ((len (length string))
        (i 0)
        (codifiable-buffer (make-string-output-stream))
        (out (make-string-output-stream)))
    (labels ((out (string)
               (write-string string out))
             (out-escaped (string)
               (out (escape-texinfo (string string))))
             (buffer-codifiable-char (char)
               (write-char char codifiable-buffer))
             (flush-codifiable-buffer ()
               (let ((codifiable (get-output-stream-string codifiable-buffer)))
                 (when (plusp (length codifiable))
                   (out (codify-and-link codifiable))))))
      (loop while (< i len)
            for char = (char string i)
            do (cond
                 ;; Escapes: \FOO
                 ((char= char #\\)
                  (flush-codifiable-buffer)
                  (incf i)
                  (when (< i len)
                    (out-escaped (char string i))
                    (incf i)
                    ;; Protect the rest of the contiguous word from
                    ;; CODIFY-AND-LINK up until
                    (loop
                      while (and (< i len)
                                 (not (or (whitespacep (char string i))
                                          (find (char string i)
                                                " ,.!?;()'[]{}\""))))
                      do (out-escaped (char string i))
                         (incf i))
                    (decf i)))
                 ;; Backticks: `CODE` with PAX downcasing and escape rules
                 ((char= char #\`)
                  (flush-codifiable-buffer)
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
                      (out "@code{")
                      (if (< slash-count 2)
                          ;; 0 or 1 backslash: Downcase if there are
                          ;; no lowercase letters (1 backslash turns
                          ;; off autolinking, which is naturally
                          ;; handled by bypassing CODIFY-AND-LINK).
                          (if (and (not (find-if #'lower-case-p actual-code))
                                   (not (find #\" actual-code)))
                              (out-escaped (maybe-downcase actual-code))
                              (out-escaped actual-code))
                          ;; 2 backslashes turn off autolinking AND downcasing.
                          (out-escaped actual-code))
                      (out "}"))))
                 ;; Strong emphasis: __foo__ -> @strong{foo}
                 ((and (char= char #\_)
                       (< (1+ i) len)
                       (char= (char string (1+ i)) #\_))
                  (let ((close-pos (search "__" string :start2 (+ i 2))))
                    (if close-pos
                        (progn
                          (flush-codifiable-buffer)
                          (format out "@strong{~A}"
                                  (process-inline-markdown
                                   (subseq string (+ i 2) close-pos)))
                          (setf i (+ close-pos 1)))
                        (buffer-codifiable-char char))))
                 ;; Emphasis: _foo_ -> @emph{foo}
                 ((char= char #\_)
                  (let ((close-pos nil)
                        (j (1+ i)))
                    (loop while (< j len)
                          do (if (char= (char string j) #\_)
                                 (if (and (< (1+ j) len)
                                          (char= (char string (1+ j)) #\_))
                                     ;; Skip double underscores so
                                     ;; they don't falsely close a
                                     ;; single underscore.
                                     (incf j 2)
                                     (progn
                                       (setf close-pos j)
                                       (return)))
                                 (incf j)))
                    (if close-pos
                        (progn
                          (flush-codifiable-buffer)
                          (format out "@emph{~A}"
                                  (process-inline-markdown
                                   (subseq string (1+ i) close-pos)))
                          (setf i close-pos))
                        (buffer-codifiable-char char))))
                 ;; Markdown autolinks: <http...> -> @url{http...}
                 ((and (char= char #\<)
                       (<= (+ i 5) len)
                       (string-equal string "http" :start1 (1+ i)
                                     :end1 (+ i 5)))
                  (let ((close-pos (position #\> string :start (1+ i))))
                    (if close-pos
                        (progn
                          (flush-codifiable-buffer)
                          (out "@url{")
                          (out-escaped (subseq string (1+ i) close-pos))
                          (out "}")
                          (setf i close-pos))
                        (buffer-codifiable-char char))))
                 ;; Markdown explicit links: [label](url) -> @uref{url, label}
                 ;;
                 ;; Markdown reflinks: [label][id] -> label
                 ((char= char #\[)
                  (let* ((close-bracket (position #\] string :start (1+ i)))
                         (next-char (when (and close-bracket
                                               (< (1+ close-bracket) len))
                                      (char string (1+ close-bracket))))
                         (open-paren (when (eql next-char #\()
                                       (1+ close-bracket)))
                         (close-paren (when open-paren
                                        (position #\) string
                                                  :start (1+ open-paren))))
                         (open-bracket2 (when (eql next-char #\[)
                                          (1+ close-bracket)))
                         (close-bracket2
                           (when open-bracket2
                             (position #\] string :start (1+ open-bracket2)))))
                    (cond
                      ;; Explicit link: [label](url)
                      (close-paren
                       (flush-codifiable-buffer)
                       (out "@uref{")
                       (out-escaped (subseq string (1+ open-paren)
                                            close-paren))
                       (out ", ")
                       (out (process-inline-markdown
                             (subseq string (1+ i) close-bracket)))
                       (out "}")
                       (setf i close-paren))
                      ;; Reflink: [label][id]
                      (close-bracket2
                       (flush-codifiable-buffer)
                       ;; Process the name, drop the id
                       (out (process-inline-markdown
                             (subseq string (1+ i) close-bracket)))
                       (setf i close-bracket2))
                      ;; Not a recognized link structure, treat as a
                      ;; normal character
                      (t
                       (buffer-codifiable-char char)))))
                 (t
                  (buffer-codifiable-char char)))
               (incf i))
      (flush-codifiable-buffer)
      (get-output-stream-string out))))

(when *downcase-uppercase-code*
  (assert (equal (process-inline-markdown "`abc`") "@code{abc}"))
  (assert (equal (process-inline-markdown "_abc_") "@emph{abc}"))
  (assert (equal (process-inline-markdown "__abc__") "@strong{abc}"))
  (assert (equal (process-inline-markdown "_PRINT_") "@emph{@code{print}}"))
  (assert (equal (process-inline-markdown "<httpabc>") "@url{httpabc}"))
  (assert (equal (process-inline-markdown "`N`") "@code{n}"))
  (assert (equal (process-inline-markdown "`N`th") "@code{n}th"))
  (assert (equal (process-inline-markdown "[x](uri)") "@uref{uri, x}"))
  (assert (equal (process-inline-markdown "[`x`](uri)") "@uref{uri, @code{x}}"))
  (assert (equal (process-inline-markdown "[function][type]") "function"))
  (assert (equal (process-inline-markdown "[`function`][type]")
                 "@code{function}")))


;;;; Processing Markdown block elements

;;; Collect lines enclosed in Markdown ``` fences. Returns the number
;;; of lines consumed and a list of lines.
(defun collect-fenced-code (lines starting-line base-indent)
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
                do (loop-finish)        ; Closing fence found
              else
                ;; Strip up to the base indentation of the environment
                do (push (if (and (indentation line)
                                  (>= (indentation line) base-indent))
                             (subseq line base-indent)
                             line)
                         result))
        (let ((env (if (string-equal lang "lisp") "lisp" "example")))
          (values consumed
                  `(,(format nil "@~A" env)
                    ,@(mapcar #'escape-texinfo (nreverse result))
                    ,(format nil "@end ~A" env))))))))

;;; Collect lines that start with a Markdown blockquote marker (">").
;;; A blockquote must be preceded by a blank line or be the first
;;; line. The marker can be indented up to 3 characters on top of
;;; BASE-INDENT. By leveraging string streams and passing the stripped
;;; content recursively back to MARKDOWN-TO-TEXINFO, we maintain full
;;; support for nested blocks, lists, and inline text wrapping.
(defun collect-blockquote (lines starting-line base-indent)
  (unless (and (> starting-line 0)
               (not (blankp (svref lines (1- starting-line)))))
    (let* ((first-line (svref lines starting-line))
           (first-indent (indentation first-line)))
      (when (and first-indent
                 (<= first-indent (+ base-indent 3))
                 (< first-indent (length first-line))
                 (char= (char first-line first-indent) #\>))
        (let ((n-lines 0)
              (stripped-lines nil))
          (loop for index from starting-line below (length lines)
                for line = (svref lines index)
                for indent = (indentation line)
                do (cond
                     ((and indent
                           (<= indent (+ base-indent 3))
                           (< indent (length line))
                           (char= (char line indent) #\>))
                      (let* ((start (1+ indent))
                             (content-start
                               (if (and (< start (length line))
                                        (char= (char line start) #\Space))
                                   (1+ start)
                                   start)))
                        (push (subseq line content-start) stripped-lines)
                        (incf n-lines)))
                     (t
                      (loop-finish))))
          ;; Trim trailing empty lines
          (loop while (and stripped-lines (string= (car stripped-lines) ""))
                do (pop stripped-lines) (decf n-lines))
          (when stripped-lines
            (let ((inner-texinfo
                    ;; Process the stripped sub-document cleanly using
                    ;; the main loop to handle paragraphs, nesting,
                    ;; and formatting automatically.
                    (with-output-to-string (*standard-output*)
                      (markdown-to-texinfo
                       (format nil "~{~A~^~%~}" (nreverse stripped-lines))
                       *texinfo-local-variables*))))
              (values n-lines
                      `("@quotation"
                        ,@(coerce (string-lines inner-texinfo) 'list)
                        "@end quotation")))))))))

;;; Collect lines indented with an extra 4 character on top of
;;; BASE-INDENT. An indented code block must be preceeded by a blank
;;; line or be the first line.
(defun collect-indented-code (lines starting-line base-indent)
  (unless (and (> starting-line 0)
               (not (blankp (svref lines (1- starting-line)))))
    (let ((indent (indentation (svref lines starting-line))))
      (when (and indent (>= indent (+ base-indent 4)))
        (let ((n-lines 0)
              (result nil))
          (loop for index from starting-line below (length lines)
                for line = (svref lines index)
                for line-indent = (indentation line)
                do (cond
                     ((blankp line)
                      ;; Blank lines are allowed inside indented code blocks.
                      (push "" result)
                      (incf n-lines))
                     ((>= line-indent (+ base-indent 4))
                      (push (subseq line (+ base-indent 4)) result)
                      (incf n-lines))
                     (t
                      ;; Indentation dropped, so the code block ends.
                      (loop-finish))))
          ;; Trim trailing empty lines.
          (loop while (and result (string= (car result) ""))
                do (pop result) (decf n-lines))
          (if result
              (values n-lines `("@example"
                                ,@(mapcar #'escape-texinfo (nreverse result))
                                "@end example"))
              nil))))))

;;; Return the indent if the line starts with a Markdown list marker
;;; (#\- or \*) followed by a space.
(defun maybe-itemize-offset (line)
  (let ((indent (indentation line)))
    (when indent
      (let ((trimmed (string-left-trim " " line)))
        (when (and (>= (length trimmed) 2)
                   (member (char trimmed 0) '(#\- #\*))
                   (char= (char trimmed 1) #\Space))
          indent)))))

;;; Collect a bulleted list.
(defun collect-markdown-itemize (lines starting-line base-indent)
  (let ((this-offset (maybe-itemize-offset (svref lines starting-line))))
    (when (and this-offset (= this-offset base-indent))
      (let ((result nil)
            (lines-consumed 0)
            (child-base (+ base-indent 4))
            (current-paragraph nil)
            (item-pending-p nil))
        (flet ((flush-paragraph ()
                 (if current-paragraph
                     (let ((processed (process-inline-markdown
                                       (format nil "~{~A~^~%~}"
                                               (nreverse current-paragraph)))))
                       (if item-pending-p
                           (push (format nil "@item ~A" processed) result)
                           (push processed result))
                       (setf current-paragraph nil)
                       (setf item-pending-p nil))
                     (when item-pending-p
                       (push "@item" result)
                       (setf item-pending-p nil)))))
          (loop for line-number = starting-line then (+ starting-line
                                                        lines-consumed)
                while (< line-number (length lines))
                for line = (svref lines line-number)
                for indent = (indentation line)
                for offset = (maybe-itemize-offset line)
                do (cond
                     ((blankp line)
                      ;; Blank lines inside lists are buffered
                      (flush-paragraph)
                      (push "" result)
                      (incf lines-consumed))
                     ;; New Item in the same list
                     ((and offset (= offset base-indent))
                      (flush-paragraph)
                      (setf item-pending-p t)
                      (let ((item-text (subseq line (+ offset 2))))
                        (unless (blankp item-text)
                          (push item-text current-paragraph)))
                      (incf lines-consumed))
                     ;; Indented block/text inside the list item (>= 4 spaces)
                     ((and indent (>= indent child-base))
                      (flush-paragraph)
                      (multiple-value-bind (sub-consumed sub-result)
                          (parse-markdown-block lines line-number child-base)
                        (if sub-consumed
                            (progn
                              (setf result (append (reverse sub-result) result))
                              (incf lines-consumed sub-consumed))
                            ;; Fallback: normal text continuing the item body
                            (progn
                              (push (subseq line child-base) current-paragraph)
                              (incf lines-consumed)))))
                     ;; Normal text continuing the item body
                     ((and indent (> indent base-indent))
                      (push line current-paragraph)
                      (incf lines-consumed))
                     ;; If we get here, the line is NOT a new bullet,
                     ;; and it less than 4 spaces of relative
                     ;; indentation, so the list is over.
                     (t
                      (loop-finish))))
          (flush-paragraph)
          ;; Trim trailing empty lines, so they return to the outer scope.
          (loop while (and result (string= (car result) ""))
                do (pop result) (decf lines-consumed))
          (values lines-consumed `("@itemize" ,@(reverse result)
                                   "@end itemize")))))))

;;; Parse the line at INDEX in LINES as a Markdown block. Return the
;;; number of lines consumed and the parse.
(defun parse-markdown-block (lines index base-indent)
  (let ((line (svref lines index)))
    (multiple-value-bind (n-lines-consumed result)
        (collect-fenced-code lines index base-indent)
      (when n-lines-consumed
        (return-from parse-markdown-block (values n-lines-consumed result))))
    (multiple-value-bind (n-lines-consumed result)
        (collect-blockquote lines index base-indent)
      (when n-lines-consumed
        (return-from parse-markdown-block (values n-lines-consumed result))))
    (cond
      ((maybe-itemize-offset line)
       (collect-markdown-itemize lines index (maybe-itemize-offset line)))
      ((and (indentation line) (>= (indentation line) (+ base-indent 4)))
       (collect-indented-code lines index base-indent))
      (t nil))))
