;;;; implementation of CL:DOCUMENTATION

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-PCL")

(defun fun-doc (function)
  (typecase function
    (interpreted-function
     #+sb-fasteval (sb-interpreter:proto-fn-docstring (sb-interpreter:fun-proto-fn function))
     #+sb-eval (sb-eval:interpreted-function-documentation function))
    (generic-function
     (slot-value function '%documentation))
    (t
     (when (closurep function)
       (let ((val (nth-value sb-impl::+closure-doc-index+
                             (closure-extra-values function))))
         (unless (unbound-marker-p val)
           (return-from fun-doc val))))
     (%simple-fun-doc (%fun-fun function)))))

(defun (setf fun-doc) (new-value function)
  (declare (type (or null string) new-value))
  (let ((new-value (canonical-docstring new-value)))
    (typecase function
      (interpreted-function
       #+sb-fasteval
       (setf (sb-interpreter:proto-fn-docstring (sb-interpreter:fun-proto-fn function))
             new-value)
       #+sb-eval
       (setf (sb-eval:interpreted-function-documentation function) new-value))
      (generic-function
       (setf (slot-value function '%documentation) new-value))
      (closure
       (set-closure-extra-values
        function nil
        (pack-closure-extra-values
         (nth-value +closure-name-index+ (closure-extra-values function))
         new-value)))
      (simple-fun
       ;; Don't allow PCL CTORs and other random functions through
       ;; because we don't want to affect builtin docstrings.
       (setf (%simple-fun-doc function) new-value))))
  new-value)

(defun real-function-name (name)
  ;; Resolve the actual name of the function named by NAME
  ;; e.g. (setf (name-function 'x) #'car)
  ;; (real-function-name 'x) => CAR
  (cond ((not (fboundp name))
         nil)
        ((and (symbolp name)
              (macro-function name))
         (let ((name (%fun-name (macro-function name))))
           (and (consp name)
                (eq (car name) 'macro-function)
                (cadr name))))
        (t
         (%fun-name (fdefinition name)))))

;;; It would be nice not to need this at all, but there's too much spaghetti
;;; and macrology for me to figure out where relying on a method just works.
(defun %doc-info (x doc-type)
  (case doc-type
    (function
     ;; Unused
     (error "FUNCTION doc-type is not supported."))
    (structure
     (typecase x
       (symbol (cond
                 ((eq (info :type :kind x) :instance)
                  (values (info :type :documentation x)))
                 ((info :typed-structure :info x)
                  (values (info :typed-structure :documentation x)))))))
    (type
     (typecase x
       (structure-class (values (info :type :documentation (class-name x))))
       (t (and (typep x 'symbol) (values (info :type :documentation x))))))
    ((t)
     (typecase x
       (function (fun-doc x))
       (structure-class (values (info :type :documentation (class-name x))))
       ((or symbol cons)
        (random-documentation x doc-type))))
    (t
     (when (typep x '(or symbol cons))
       (random-documentation x doc-type)))))

;;; (SETF %DOC-INFO) is a thin wrapper on INFO that set or clears
;;; a :DOCUMENTATION info value depending on whether STRING is NIL.
;;; It, and the corresponding reader, are not for use outside this file.
(defun (setf %doc-info) (string name doc-type)
  (declare (type (or null string) string))
  (let ((string (canonical-docstring string))
        (info-number
          (macrolet ((info-number (class type)
                       (meta-info-number (meta-info class type))))
            (case doc-type
              (variable (info-number :variable :documentation))
              (structure
               (cond ((eq (info :type :kind name) :instance)
                      (info-number :type :documentation))
                     ((info :typed-structure :info name)
                      (info-number :typed-structure :documentation))))
              (type (info-number :type :documentation))))))
    (cond (info-number
           (if string
               (set-info-value name info-number string)
               (clear-info-values name (list info-number))))
          ((eq doc-type 'function)
           ;; FIXME: this silently loses
           ;; * (setf (documentation '(a bad name) 'function) "x") => "x"
           ;; * (documentation '(a bad name) 'function) => NIL
           ;; which is fine because as noted in pcl/documentation.lsp
           ;;   even for supported doc types an implementation is permitted
           ;;   to discard docs at any time
           ;; but should a warning be issued just as for an unknown DOC-TYPE?
           ;;
           ;; And there's additional weirdness if you do, in this order -
           ;;  * (setf (documentation 'foo 'function) "hi")
           ;;  * (defun foo () "hey" 1)
           ;;  * (documentation 'foo 'function) => "hi" ; should be "hey"
           ;; CLHS says regarding DEFUN:
           ;; " Documentation is attached as a documentation string to
           ;;   /name/ (as kind function) and to the /function object/."
           (cond ((not (legal-fun-name-p name)))
                 ((not (equal (real-function-name name) name))
                  (setf (random-documentation name 'function) string))
                 (t
                  (setf (fun-doc (fdefinition name)) string))))
          ((typep name '(or symbol cons))
           (setf (random-documentation name doc-type) string))))
  string)

(defun set-function-name-documentation (name documentation)
  (aver name)
  (cond ((not (legal-fun-name-p name))
         nil)
        ((not (equal (real-function-name name) name))
         (setf (random-documentation name 'function) documentation))
        (t
         (setf (fun-doc (or (and (symbolp name)
                                 (macro-function name))
                            (fdefinition name)))
               documentation)))
  documentation)

;;; Generic behavior

(defgeneric documentation (object doc-type)
  (:argument-precedence-order doc-type object))

(fmakunbound '(setf documentation))
(defgeneric (setf documentation) (new-value object doc-type)
  (:argument-precedence-order doc-type object new-value))

(defmethod (setf documentation) :around (new-value (x (eql nil)) doc-type)
  (style-warn "Ignoring doc-type ~a for ~a." doc-type nil)
  new-value)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod documentation (object doc-type)
  (warn "unsupported DOCUMENTATION: doc-type ~S for object of type ~S"
        doc-type (type-of object))
  nil)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod (setf documentation) (new-value object doc-type)
  ;; CMU CL made this an error, but since ANSI says that even for supported
  ;; doc types an implementation is permitted to discard docs at any time
  ;; for any reason, this feels to me more like a warning. -- WHN 19991214
  (warn "discarding unsupported DOCUMENTATION: doc-type ~S for object of type ~S"
        doc-type (type-of object))
  new-value)

;;; Deprecation note

(defun maybe-add-deprecation-note (namespace name documentation)
  (unless (member namespace '(function variable type))
    (return-from maybe-add-deprecation-note documentation))
  (binding* (((state since replacements)
              (deprecated-thing-p namespace name))
             (note (when state
                     (%with-output-to-string (stream)
                       (sb-impl::print-deprecation-message
                        namespace name (first since) (second since)
                        replacements stream)))))
    (cond
      ((and documentation note)
       (concatenate
        'string note #.(format nil "~2%") documentation))
      (documentation)
      (note))))

(defmethod documentation :around ((x t) (doc-type t))
  (let ((namespace (cond
                     ((typep x 'function)
                      'function)
                     ((eq doc-type 'compiler-macro)
                      'function)
                     ((typep x 'class)
                      'type)
                     ((eq doc-type 'structure)
                      'type)
                     (t
                      doc-type)))
        (name (cond
                ((typep x 'function)
                 (%fun-name x))
                ((typep x 'class)
                 (class-name x))
                (t
                 x)))
        (documentation (call-next-method)))
    (maybe-add-deprecation-note namespace name
                                (normalize-sbcl-docstring x name doc-type
                                                          documentation))))

(defvar *normalize-sbcl-docstrings* t)

(defun normalize-sbcl-docstring (object name doc-type docstring)
  (if (and *normalize-sbcl-docstrings*
           docstring
           (sbcl-definition-name-p object name doc-type))
      (string-right-trim '(#\Newline) (markdown-to-plain-text
                                       (reindent-docstring docstring)))
      docstring))

(defun non-setf-name (name)
  (if (and (consp name)
           (eq (first name) 'setf)
           (consp (cdr name)))
      (second name)
      name))

(defun sbcl-package-name-p (name)
  (when (stringp name)
    (or (string= name "COMMON-LISP")
        (and (>= (length name) 3)
             (string= name "SB-" :end1 3)))))

(defun sbcl-definition-name-p (object name doc-type)
  (and (member doc-type '(t compiler-macro function method-combination setf
                          structure type variable declaration))
       (if (and (packagep object) (eq doc-type t))
           (sbcl-package-name-p (package-name name))
           (let ((name (non-setf-name name)))
             (if (null name)
                 (null object)
                 (when (symbolp name)
                   (let ((package (symbol-package name)))
                     (when package
                       (sbcl-package-name-p (package-name package))))))))))

;;; functions, macros, and special forms

(flet ((maybe-function-documentation (name)
         (cond
           ((not (legal-fun-name-p name))
            nil)
           ((random-documentation name 'function))
           ;; Nothing under the name, check the function object.
           ((fboundp name)
            (fun-doc (cond
                       ((and (symbolp name) (special-operator-p name))
                        (fdefinition name))
                       ((and (symbolp name) (macro-function name)))
                       ((fdefinition name)
                        (sb-ext:unencapsulated-function name))))))))

  (defmethod documentation ((x function) (doc-type (eql 't)))
    (fun-doc x))

  (defmethod documentation ((x function) (doc-type (eql 'function)))
    (fun-doc x))

  (defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
    (awhen (compiler-macro-function x)
      (documentation it t)))

  (defmethod documentation ((x list) (doc-type (eql 'function)))
    (maybe-function-documentation x))

  (defmethod documentation ((x symbol) (doc-type (eql 'function)))
    (maybe-function-documentation x))

  (defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
    (awhen (compiler-macro-function x)
      (documentation it t))))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 'function)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (set-function-name-documentation x (canonical-docstring new-value))
  new-value)

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  (set-function-name-documentation x (canonical-docstring new-value))
  new-value)

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

;;; SETF documentation is attached to the function that performs expansion,
;;; except for short form DEFSETF which is in the globaldb value directly.
(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (let ((expander (info :setf :expander x))
        (new-value (canonical-docstring new-value)))
    (typecase expander
      ((cons symbol) (setf (second expander) new-value))
      (cons (setf (documentation (cdr expander) 'function) new-value))
      (function (setf (documentation expander 'function) new-value))))
  new-value)

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (let ((expander (info :setf :expander x)))
    (typecase expander
      ((cons symbol) (second expander))
      (cons (documentation (cdr expander) 'function))
      (function (documentation expander 'function)))))

;;; method combinations
(defmethod documentation ((x method-combination) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod documentation
    ((x method-combination) (doc-type (eql 'method-combination)))
  (slot-value x '%documentation))

(defmethod documentation ((x symbol) (doc-type (eql 'method-combination)))
  (random-documentation x 'method-combination))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) (canonical-docstring new-value))
  new-value)

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 'method-combination)))
  (setf (slot-value x '%documentation) (canonical-docstring new-value))
  new-value)

(defmethod (setf documentation)
    (new-value (x symbol) (doc-type (eql 'method-combination)))
  (setf (random-documentation x 'method-combination) new-value))

;;; methods
(defmethod documentation ((x standard-method) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod (setf documentation)
    (new-value (x standard-method) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) (canonical-docstring new-value))
  new-value)

;;; types, classes, and structure names

(macrolet
    ((define-type-documentation-methods (specializer get-form set-form)
       `(progn
          (defmethod documentation ((x ,specializer) (doc-type (eql 't)))
            ,get-form)

          (defmethod documentation ((x ,specializer) (doc-type (eql 'type)))
            (documentation x t))

          (defmethod (setf documentation) (new-value
                                           (x ,specializer)
                                           (doc-type (eql 't)))
            ,set-form)

          (defmethod (setf documentation) (new-value
                                           (x ,specializer)
                                           (doc-type (eql 'type)))
            (setf (documentation x 't) new-value))))
     (define-type-documentation-lookup-methods (doc-type)
       `(progn
          (defmethod documentation ((x symbol) (doc-type (eql ',doc-type)))
            (acond
             ((find-class x nil)
              (documentation it t))
             (t
              (%doc-info x ',doc-type))))

          (defmethod (setf documentation) (new-value
                                           (x symbol)
                                           (doc-type (eql ',doc-type)))
            (let ((new-value (canonical-docstring new-value)))
             (acond
               ((find-class x nil)
                (setf (documentation it t) new-value))
               (t
                (setf (%doc-info x ',doc-type) new-value))))
            new-value))))

  (define-type-documentation-methods structure-class
      (%doc-info (class-name x) 'type)
      (setf (%doc-info (class-name x) 'type) new-value))

  (define-type-documentation-methods class
      (slot-value x '%documentation)
      (setf (slot-value x '%documentation) (canonical-docstring new-value)))

  ;; although the CLHS doesn't mention this, it is reasonable to
  ;; assume that parallel treatment of condition-class was intended
  ;; (if condition-class is in fact not implemented as a
  ;; standard-class or structure-class).
  (define-type-documentation-methods condition-class
      (%doc-info (class-name x) 'type)
      (setf (%doc-info (class-name x) 'type) new-value))

  (define-type-documentation-lookup-methods type)
  (define-type-documentation-lookup-methods structure))


;;; variables
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (values (info :variable :documentation x)))

(defmethod (setf documentation) (new-value
                                 (x symbol)
                                 (doc-type (eql 'variable)))
  (setf (%doc-info x 'variable) new-value))

;;; extra-standard methods, for getting at slot documentation
(defmethod documentation ((slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (slot-value slotd '%documentation))

(defmethod (setf documentation)
    (new-value (slotd standard-slot-definition) (doc-type (eql 't)))
  (setf (slot-value slotd '%documentation) (canonical-docstring new-value)))

;;; declarations
(defmethod documentation ((x symbol) (doc-type (eql 'declaration)))
  (values (info :declaration :documentation x)))

(defmethod (setf documentation) (new-value (x symbol)
                                 (doc-type (eql 'declaration)))
  (if new-value
      (setf (info :declaration :documentation x) new-value)
      (clear-info :declaration :documentation x)))

;;; Now that we have created the machinery for setting documentation, we can
;;; set the documentation for the machinery for setting documentation.
(setf (documentation 'documentation 'function)
      "Return the documentation string of DOC-TYPE for OBJECT,
or NIL if none exists. In addition to the DOC-TYPEs and methods
required by ANSI, SBCL's DOCUMENTATION (and its SETF) supports methods
with the following signatures:

- `(OBJECT SYMBOL) (DOC-TYPE (EQL DECLARATION))`

- `(OBJECT SB-MOP:SLOT-DEFINITION) (DOC-TYPE (EQL T))`

Since CONDITIONs are implemented as classes in SBCL, the following
also work:

- `(OBJECT CONDITION) (DOC-TYPE (EQL T))`

- `(OBJECT CONDITION) (DOC-TYPE (EQL 'TYPE))`

Function documentation is stored separately for function names and objects:
DEFUN, LAMBDA, &co create function objects with the specified documentation
strings.

    (setf (documentation name 'function) string)

sets the documentation string stored under the specified name, and

    (setf (documentation func t) string)

sets the documentation string stored in the function object.

    (documentation name 'function)

returns the documentation stored under the function name if any, and
falls back on the documentation in the function object if necessary.")

(setf (documentation '+slot-unbound+ 'variable)
  "SBCL specific extensions to MOP: if this value is read from an
instance using STANDARD-INSTANCE-ACCESS, the slot is unbound.
Similarly, an :INSTANCE allocated slot can be made unbound by
assigning this to it using (SETF STANDARD-INSTANCE-ACCESS).

Value of +SLOT-UNBOUND+ is unspecified, and should not be relied to be
of any particular type, but it is guaranteed to be suitable for EQ
comparison.")

(!install-cross-compiled-methods 'documentation)
(!install-cross-compiled-methods '(setf documentation))

(dolist (args (prog1 *!docstrings* (makunbound '*!docstrings*)))
  (apply #'(setf documentation) args))

;;;; Markdown to plain text along the lines of SB-MANUAL::MARKDOWN-TO-TEXINFO

(defun markdown-to-plain-text (string)
  (markdown-lines-to-plain-text (string-lines string) 0))

(defun markdown-lines-to-plain-text (lines base-indent)
  (with-output-to-string (out)
    (let ((buf nil))
      (labels ((flush ()
                 (write-md-paragraph buf out)
                 (setq buf nil)))
        (loop for l below (length lines)
              for line = (svref lines l)
              do (let ((n (write-md-block lines l base-indent out #'flush)))
                   (if n
                       (incf l (1- n))
                       (push line buf))))
        (flush)))))

(defun string-lines (string)
  (coerce (with-input-from-string (s string)
            (loop for line = (read-line s nil nil)
                  while line collect line))
          'vector))

(defun whitespacep (char)
  (find char #(#\Tab #\Space #\Page #\Newline #\Return)))

(defun indentation (line)
  (position-if-not #'whitespacep line))

(defun blankp (line)
  (null (indentation line)))

(defun write-md-paragraph (reversed-lines out)
  (when reversed-lines
    (let* ((str (format nil "~{~A~^~%~}" (reverse reversed-lines)))
           (len (length str))
           (bound t))
      (loop for i below len
            for c = (char str i)
            do (cond
                 ((and bound (char= c #\\))
                  (when (and (< (1+ i) len)
                             (char= (char str (1+ i)) #\\))
                    (incf i))
                  (setf bound nil))
                 ((char= c #\`)
                  (incf i)
                  (loop repeat 2
                        while (< i len)
                        while (char= (char str i) #\\)
                        do (incf i))
                  (loop while (< i len)
                        while (char/= (char str i) #\`)
                        do (write-char (char str i) out)
                           (incf i))
                  (setq bound nil))
                 (t
                  (write-char c out)
                  (setq bound (whitespacep c)))))
      (terpri out))))

(defun write-md-block (lines index base-indent out flush-fn)
  (or (write-md-fenced-code lines index base-indent out flush-fn)
      (write-md-indented-code lines index base-indent out flush-fn)
      (write-md-blockquote lines index base-indent out flush-fn)
      (write-md-markdown-itemize lines index base-indent out flush-fn)))

(defun write-md-fenced-code (lines start base-indent out flush-fn)
  (declare (ignore base-indent))
  (let* ((line (svref lines start))
         (i (indentation line)))
    (when (and i (>= (length line) (+ i 3))
               (string= line "```" :start1 i :end1 (+ i 3)))
      (funcall flush-fn)
      (loop for l from (1+ start) below (length lines)
            for line = (svref lines l)
            for ind = (indentation line)
            if (and ind (>= (length line) (+ ind 3))
                    (string= line "```" :start1 ind :end1 (+ ind 3)))
              do (return (- (1+ l) start))
            else do (write-line line out)
            finally (return (- l start))))))

(defun write-md-indented-code (lines start base-indent out flush-fn)
  (when (or (zerop start)
            (blankp (svref lines (1- start))))
    (let ((i (indentation (svref lines start))))
      (when (and i (>= i (+ base-indent 4)))
        (funcall flush-fn)
        (loop for l from start below (length lines)
              for line = (svref lines l)
              for ind = (indentation line)
              while (or (null ind)
                        (>= ind (+ base-indent 4)))
              do (write-line line out)
              finally (return (- l start)))))))

(defun write-md-blockquote (lines start base-indent out flush-fn)
  (when (or (zerop start)
            (blankp (svref lines (1- start))))
    (let ((i (indentation (svref lines start))))
      (when (and i (<= i (+ base-indent 3))
                 (char= (char (svref lines start) i) #\>))
        (funcall flush-fn)
        (let (stripped prefixes)
          (loop for l from start below (length lines)
                for line = (svref lines l)
                for ind = (indentation line)
                while (and ind (<= ind (+ base-indent 3))
                           (char= (char line ind) #\>))
                do (let ((c-start (if (and (< (1+ ind) (length line))
                                           (char= (char line (1+ ind)) #\Space))
                                      (+ ind 2) (1+ ind))))
                     (push (subseq line c-start) stripped)
                     (push (subseq line 0 c-start) prefixes)))
          (let ((inner (markdown-lines-to-plain-text
                        (coerce (nreverse stripped) 'vector) 0)))
            (loop for prefix in (nreverse prefixes)
                  for line across (string-lines inner)
                  do (write-string prefix out)
                     (write-line line out)))
          (length prefixes))))))

(defun maybe-itemize-offset (line)
  (let ((i (indentation line)))
    (when (and i (< (1+ i) (length line))
               (find (char line i) "-*")
               (char= (char line (1+ i)) #\Space))
      i)))

(defun write-md-markdown-itemize (lines start base-indent out flush-fn)
  (when (eql (maybe-itemize-offset (svref lines start)) base-indent)
    (funcall flush-fn)
    (let ((child (+ base-indent 4))
          (buf nil))
      (labels ((flush ()
                 (write-md-paragraph buf out)
                 (setq buf nil)))
        (loop for l from start below (length lines)
              for line = (svref lines l)
              for ind = (indentation line)
              do (cond ((blankp line)
                        (flush)
                        (write-line line out))
                       ((eql (maybe-itemize-offset line) base-indent)
                        (flush)
                        (push line buf))
                       ((>= ind child)
                        (let ((n (write-md-block lines l child out #'flush)))
                          (if n
                              (incf l (1- n))
                              (push line buf))))
                       ((> ind base-indent)
                        (push line buf))
                       (t
                        (loop-finish)))
              finally (flush)
                      (return (- l start)))))))


;;; Normalize docstring indentation by stripping the longest run of
;;; leading spaces common to all non-blank lines except the first.
(defun reindent-docstring (docstring)
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
                                   (subseq line (min (length line)
                                                     indentation)))
                               out)
                 (unless missing-newline-p
                   (terpri out)))))))
