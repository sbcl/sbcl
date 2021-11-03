;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-FORMAT")

;;;; TOKENIZE-CONTROL-STRING

;;; The case for caching is to speed up out-of-line calls that use a fixed
;;; control string in a loop, not to avoid re-tokenizing all strings that
;;; happen to be STRING= to that string.
;;; (Might we want to bypass the cache when compile-time tokenizing?)
#+sb-xc-host
(defun tokenize-control-string (string)
  (combine-directives
   (%tokenize-control-string string 0 (length string) nil)
   t))
#-sb-xc-host
(defun-cached (tokenize-control-string
               :memoizer memoize
               :hash-bits 7
               :hash-function #'pointer-hash)
              ((string eq))
  (declare (simple-string string))
  (macrolet ((compute-it ()
               `(combine-directives
                 (%tokenize-control-string string 0 (length string) nil)
                 t)))
    (if (logtest (get-header-data string)
                 ;; shareable = readonly
                 (ash (logior sb-vm:+vector-shareable+
                              sb-vm:+vector-shareable-nonstd+)
                      sb-vm:array-flags-data-position))
        (memoize (compute-it))
        (compute-it))))

;;; If at some point I can figure out how to *CORRECTLY* utilize
;;; non-simple strings, then the INDEX and END will bound the parse.
;;; [Tokenization is the easy part, it's the substring extraction
;;; and processing, and error reporting, that become very complicated]
(defun %tokenize-control-string (string index end symbols)
  (declare (simple-string string))
  (let ((result nil)
        ;; FIXME: consider rewriting this 22.3.5.2-related processing
        ;; using specials to maintain state and doing the logic inside
        ;; the directive expanders themselves.
        (block)
        (pprint)
        (semicolon)
        (justification-semicolon))
    (loop
      (let ((next-directive (or (position #\~ string :start index :end end) end)))
        (when (> next-directive index)
          (push (possibly-base-stringize (subseq string index next-directive))
                result))
        (when (= next-directive end)
          (return))
        (let* ((directive (parse-directive string next-directive symbols))
               (char (directive-character directive)))
          ;; this processing is required by CLHS 22.3.5.2
          (cond
            ((char= char #\<) (push directive block))
            ((and block (char= char #\;) (directive-colonp directive))
             (setf semicolon directive))
            ((char= char #\>)
             (unless block
               (format-error-at string next-directive
                                "~~> without a matching ~~<"))
             (cond
               ((directive-colonp directive)
                (unless pprint
                  (setf pprint (car block)))
                (setf semicolon nil))
               (semicolon
                (unless justification-semicolon
                  (setf justification-semicolon semicolon))))
             (pop block))
            ;; block cases are handled by the #\< expander/interpreter
            ((not block)
             (case char
               ((#\W #\I #\_) (unless pprint (setf pprint directive)))
               (#\T (when (and (directive-colonp directive)
                               (not pprint))
                      (setf pprint directive))))))
          (push directive result)
          (when (char= (directive-character directive) #\/)
            (pop symbols))
          (setf index (directive-end directive)))))
    (when (and pprint justification-semicolon)
      (let ((pprint-offset (1- (directive-end pprint)))
            (justification-offset
             (1- (directive-end justification-semicolon))))
        (format-error-at*
         string (min pprint-offset justification-offset)
         "Misuse of justification and pprint directives" '()
         :second-relative (- (max pprint-offset justification-offset)
                             (min pprint-offset justification-offset)
                             1)
         :references '((:ansi-cl :section (22 3 5 2))))))
    (nreverse result)))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun parse-directive (string start symbols)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
        (end (length string)))
    (flet ((get-char ()
             (if (= posn end)
                 (format-error-at string start
                                  "String ended before directive was found")
                 (schar string posn)))
           (check-ordering ()
             (when (or colonp atsignp)
               (format-error-at*
                string posn
                "Parameters found after #\\: or #\\@ modifier" '()
                :references '((:ansi-cl :section (22 3)))))))
      (loop
        (let ((char (get-char)))
          (cond ((or (char<= #\0 char #\9) (char= char #\+) (char= char #\-))
                 (check-ordering)
                 (multiple-value-bind (param new-posn)
                     (parse-integer string :start posn :junk-allowed t)
                   (push (cons posn param) params)
                   (setf posn new-posn)
                   (case (get-char)
                     (#\,)
                     ((#\: #\@)
                      (decf posn))
                     (t
                      (return)))))
                ((or (char= char #\v)
                     (char= char #\V))
                 (check-ordering)
                 (push (cons posn :arg) params)
                 (incf posn)
                 (case (get-char)
                   (#\,)
                   ((#\: #\@)
                    (decf posn))
                   (t
                    (return))))
                ((char= char #\#)
                 (check-ordering)
                 (push (cons posn :remaining) params)
                 (incf posn)
                 (case (get-char)
                   (#\,)
                   ((#\: #\@)
                    (decf posn))
                   (t
                    (return))))
                ((char= char #\')
                 (check-ordering)
                 (incf posn)
                 (push (cons posn (get-char)) params)
                 (incf posn)
                 (unless (char= (get-char) #\,)
                   (decf posn)))
                ((char= char #\,)
                 (check-ordering)
                 (push (cons posn nil) params))
                ((char= char #\:)
                 (if colonp
                     (format-error-at*
                      string posn "Too many colons supplied" '()
                      :references '((:ansi-cl :section (22 3))))
                     (setf colonp t)))
                ((char= char #\@)
                 (if atsignp
                     (format-error-at*
                      string posn "Too many #\\@ characters supplied" '()
                      :references '((:ansi-cl :section (22 3))))
                     (setf atsignp t)))
                (t
                 (when (and (char= (schar string (1- posn)) #\,)
                            (or (< posn 2)
                                (char/= (schar string (- posn 2)) #\')))
                   (check-ordering)
                   (push (cons (1- posn) nil) params))
                 (return))))
        (incf posn))
      (let ((char (get-char)))
        (when (char= char #\/)
          (let ((closing-slash (position #\/ string :start (1+ posn))))
            (if closing-slash
                (setf posn closing-slash)
                (format-error-at string posn "No matching closing slash"))))
        (make-format-directive
         string start (1+ posn)
         (nreverse params) colonp atsignp (char-upcase char)
         (when (eql char #\/) (car symbols))))))))

;;;; specials used to communicate information

;;; Used both by the expansion stuff and the interpreter stuff. When it is
;;; non-NIL, up-up-and-out (~:^) is allowed. Otherwise, ~:^ isn't allowed.
(defvar *up-up-and-out-allowed* nil)

;;; Used by the interpreter stuff. When it's non-NIL, it's a function
;;; that will invoke PPRINT-POP in the right lexical environemnt.
(defvar *logical-block-popper* nil)
(declaim (type (or null function) *logical-block-popper*)
         (always-bound *logical-block-popper*))

;;; Used by the expander stuff. This is bindable so that ~<...~:>
;;; can change it.
(defvar *expander-next-arg-macro* 'expander-next-arg)

;;; Used by the expander stuff. Initially starts as T, and gets set to NIL
;;; if someone needs to do something strange with the arg list (like use
;;; the rest, or something).
(defvar *only-simple-args*)

;;; Used by the expander stuff. We do an initial pass with this as NIL.
;;; If someone doesn't like this, they (THROW 'NEED-ORIG-ARGS NIL) and we try
;;; again with it bound to T. If this is T, we don't try to do anything
;;; fancy with args.
(defvar *orig-args-available* nil)

;;; Used by the expander stuff. List of (symbol . offset) for simple args.
(defvar *simple-args*)

;;; Make a few simplifications to the directive list in INPUT,
;;; including translation of ~% to literal newline.
;;; I think that this does not have implications on conditional newlines
;;; vis a vis "When a line break is inserted by any type of conditional
;;; newline, any blanks that immediately precede the conditional newline
;;; are omitted". i.e. one could argue that nonliteral blanks are not
;;; quite the same as literal blanks, i.e. not subject to removal,
;;; but I don't think that's true, and in fact that is the source of
;;; an extremely subtle bug that writing the #\space character as a
;;; physical space character followed by a conditional newline is,
;;; if taken literally, supposed to remove the desired output.
(defun combine-directives (input literalize-tilde)
  (let (output)
    (labels ((emit-string (string)
               (if (stringp (car output))
                   (rplaca output (concat (car output) string))
                   (push string output)))
             (emit-placeholder-p ()
               ;; Return T if an otherwise empty string inside ~{ ... ~} must be
               ;; kept in order to avoid turning "~{~0|~}" (etc) into "~{~}".
               ;; The latter control string means something completely different.
               (and (format-directive-p (car output))
                    (eql #\{ (directive-character (car output)))))
             (emit-directive (directive)
               ;; If the head of output is the empty string and DIRECTIVE is
               ;; NOT a closing curly brace, we can reuse the first cons cell.
               (if (and (typep (car output) '(string 0))
                        (char/= (directive-character directive) #\}))
                   (rplaca output directive)
                   (push directive output)))
             (concat (first second)
               ;; The result is a base-string iff both inputs are base-string.
               ;; %TOKENIZE-CONTROL-STRING calls POSSIBLY-BASE-STRINGIZE
               ;; on all pieces of the input.
               ;; I suspect that this may be permissible for CONCATENATE generally
               ;; when given 'STRING as its output type with base-string as inputs.
               ;; But as with all things string, the spec is unclear as usual imho.
               (cond ((and (typep first 'base-string)
                           (typep second 'base-string))
                      (concatenate 'base-string first second))
                     (t
                      (concatenate 'string first second)))))
      (loop
        (unless input (return (nreverse output)))
        (let ((item (pop input)))
          (etypecase item
            (string
             (aver (plusp (length item)))
             (emit-string item))
            ;;  - Handle tilde-newline immediately
            ;;  - Turn "~%" into literal newline (for parameter N <=127)
            ;;  - Unless x-compiling, turn "~|" into literal form-feed (ditto)
            ;;  - Optionally turn "~~" into literal tilde (ditto)
            (format-directive
             (let ((params (directive-params item))
                   (colon (directive-colonp item))
                   (atsign (directive-atsignp item))
                   (char (directive-character item)))
               (block nil
                 (case char
                   (#\Newline
                    ;; tilde newline wants no params, and not both colon+atsign
                    (when (and (not params) (not (and colon atsign)))
                      ;; atsign = preserve newline / colon = preserve whitespace
                      (let ((s (concat (if atsign
                                           #.(make-string 1 :initial-element #\Newline)
                                           "")
                                       (if (and (not colon) (stringp (car input)))
                                           (string-left-trim
                                            ;; #\Tab is a nonstandard char
                                            `(#-sb-xc-host ,(code-char tab-char-code)
                                              #\space #\newline)
                                            (pop input))
                                           ""))))
                        (when (or (plusp (length s)) (emit-placeholder-p))
                          (emit-string s)))
                      (return)))
                   ;; TODO: #\& with a literal parameter of 0 is equivalent
                   ;; to the empty string.
                   ((#\% #\~ #-sb-xc-host #\|) ; #\Page is a nonstandard char
                    (let ((n (or (cdar params) 1)))
                      (when (and (not (or colon atsign))
                                 (or (null params) (singleton-p params))
                                 (typep n '(mod 128))
                                 ;; Don't insert literal tilde when parsing/
                                 ;; unparsing to create a FMT-CONTROL instance.
                                 (or (not (eql char #\~)) literalize-tilde))
                        (when (or (plusp n) (emit-placeholder-p))
                          (let ((char (case char
                                        (#\% #\Newline)
                                        #-sb-xc-host (#\| (code-char form-feed-char-code))
                                        (t char))))
                            (emit-string (make-string n :initial-element char))))
                        (return)))))
                 (emit-directive item))))))))))

;;;; FORMATTER stuff

(sb-xc:defmacro formatter (control-string)
  `#',(%formatter control-string))

(defun %formatter (control-string &optional (arg-count 0) (need-retval t)
                                  &aux (lambda-name
                                        (possibly-base-stringize
                                         (concatenate 'string "fmt$" control-string))))
  ;; ARG-COUNT is supplied only when the use of this formatter is in a literal
  ;; call to FORMAT, in which case we can possibly elide &optional parsing.
  ;; But we can't in general, because FORMATTER may be called by users
  ;; to obtain functions that may be invoked in random wrong ways.
  ;; NEED-RETVAL signifies that the caller wants back the list of
  ;; unconsumed arguments. This is the default assumption.
  (block nil
    (catch 'need-orig-args
      (let* ((*simple-args* nil)
             (*only-simple-args* t)
             (control-string (coerce control-string 'simple-string))
             (guts (expand-control-string control-string)) ; can throw
             (required nil)
             (optional nil))
        (dolist (arg *simple-args*)
          (cond ((plusp arg-count)
                 (push (car arg) required)
                 (decf arg-count))
                (t
                 (push `(,(car arg)
                         (args-exhausted ,control-string ,(cdr arg)))
                       optional))))
        (return `(named-lambda ,lambda-name
                         (stream ,@required
                                 ,@(if optional '(&optional)) ,@optional
                                 &rest args)
                   (declare (ignorable stream args))
                   ,guts
                   ,(and need-retval 'args)))))
    (let ((*orig-args-available* t)
          (*only-simple-args* nil))
      `(named-lambda ,lambda-name (stream &rest orig-args)
         (declare (ignorable stream) (muffle-conditions compiler-note))
         (let ((args orig-args))
           ,(expand-control-string control-string)
           ,(and need-retval 'args))))))

(defun args-exhausted (control-string offset)
  (format-error-at control-string offset "No more arguments"))

(defvar *format-gensym-counter*)
(defun expand-control-string (string)
  (let* ((string (etypecase string
                   (simple-string
                    string)
                   (string
                    (coerce string 'simple-string))))
         (*default-format-error-control-string* string)
         (*format-gensym-counter* 0)
         (directives (tokenize-control-string string)))
    `(block nil
       ,@(expand-directive-list directives))))

(defun expand-directive-list (directives)
  (let ((results nil)
        previous
        (remaining-directives directives))
    (loop
     (unless remaining-directives
       (return))
     (multiple-value-bind (form new-directives)
         (expand-directive (car remaining-directives)
                           (cdr remaining-directives))
       (flet ((merge-string (string)
                (cond (previous
                       ;; It would be nice if (CONCATENTE 'STRING)
                       ;; could return the "smallest" string type
                       ;; able to hold the result. Or at least if we
                       ;; had a better interface than wrapping
                       ;; it with POSSIBLY-BASE-STRINGIZE since that
                       ;; conses two new strings usually.
                       (let ((concat
                              (possibly-base-stringize
                               (concatenate 'string
                                            (string previous)
                                            (string string)))))
                         (setf previous concat)
                         (setf (car results)
                               `(write-string ,concat stream))))
                      (t
                       (setf previous string)
                       (push form results)))))
         (cond ((not form))
               ((typep form '(cons (member write-string write-char)
                                   (cons (or string character))))
                (merge-string (second form)))
               ((typep form '(cons (eql terpri)))
                (merge-string #\Newline))
               (t
                (push form results)
                (setf previous nil))))
       (setf remaining-directives new-directives)))
    (nreverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
            (aref *format-directive-expanders* (directive-code directive)))
           (*default-format-error-offset*
            (1- (directive-end directive))))
       (if (functionp expander)
           (funcall expander directive more-directives)
           (format-error "Unknown directive ~@[(character: ~A)~]"
                         (directive-char-name directive)))))
    ((simple-string 1)
     (values `(write-char ,(schar directive 0) stream)
             more-directives))
    (simple-string
     (values `(write-string ,directive stream)
             more-directives))))

(sb-xc:defmacro expander-next-arg (string offset)
  `(if args
       (pop args)
       (format-error-at ,string ,offset "No more arguments")))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(,*expander-next-arg-macro*
        ,*default-format-error-control-string*
        ,(or offset *default-format-error-offset*))
      (let ((symbol
             (without-package-locks
                 (package-symbolicate
                  #.(find-package "SB-FORMAT")
                  "FORMAT-ARG"
                  (write-to-string (incf *format-gensym-counter*)
                                   :pretty nil :base 10 :radix nil)))))
        (push (cons symbol (or offset *default-format-error-offset*))
              *simple-args*)
        symbol)))

(defmacro expand-bind-defaults (specs params &body body)
  (once-only ((params params))
    (if specs
        (collect ((expander-bindings) (runtime-bindings))
          (dolist (spec specs)
            (destructuring-bind (var default) spec
              (let ((symbol (sb-xc:gensym "FVAR")))
                (expander-bindings
                 `(,var ',symbol))
                (runtime-bindings
                 `(list ',symbol
                   (let* ((param-and-offset (pop ,params))
                          (offset (car param-and-offset))
                          (param (cdr param-and-offset)))
                     (case param
                       (:arg `(or ,(expand-next-arg offset) ,,default))
                       (:remaining
                        (setf *only-simple-args* nil)
                        '(length args))
                       ((nil) ,default)
                       (t param))))))))
          `(let ,(expander-bindings)
            `(let ,(list ,@(runtime-bindings))
              ,@(if ,params
                    (format-error-at
                     nil (caar ,params)
                     "Too many parameters, expected no more than ~W"
                     ,(length specs)))
              ,,@body)))
        `(progn
           (when ,params
             (format-error-at nil (caar ,params)
                              "Too many parameters, expected none"))
           ,@body))))

;;;; format directive machinery

(defmacro def-complex-format-directive (char lambda-list &body body)
  (let ((defun-name (intern (format nil
                                    "~:@(~:C~)-FORMAT-DIRECTIVE-EXPANDER"
                                    char)))
        (directive (sb-xc:gensym "DIRECTIVE"))
        (directives (if lambda-list (car (last lambda-list)) (sb-xc:gensym "DIRECTIVES"))))
    `(progn
       (defun ,defun-name (,directive ,directives)
         ,@(if lambda-list
               `((let ,(mapcar (lambda (var)
                                 `(,var
                                   (,(symbolicate "DIRECTIVE-" var) ,directive)))
                               (butlast lambda-list))
                   ,@body))
               `((declare (ignore ,directive ,directives))
                 ,@body)))
       (%set-format-directive-expander ,char #',defun-name))))

(defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (sb-xc:gensym "DIRECTIVES"))
        (declarations nil)
        (body-without-decls body))
    (loop
      (let ((form (car body-without-decls)))
        (unless (and (consp form) (eq (car form) 'declare))
          (return))
        (push (pop body-without-decls) declarations)))
    (setf declarations (reverse declarations))
    `(def-complex-format-directive ,char (,@lambda-list ,directives)
       ,@declarations
       (values (progn ,@body-without-decls)
               ,directives))))

(defun %set-format-directive-expander (char fn)
  (let ((code (char-code (char-upcase char))))
    (setf (aref *format-directive-expanders* code) fn))
  char)

(defun find-directive (directives kind stop-at-semi)
  (if directives
      (let ((next (car directives)))
        (if (format-directive-p next)
            (let ((char (directive-character next)))
              (if (or (char= kind char)
                      (and stop-at-semi (char= char #\;)))
                  (car directives)
                  (find-directive
                   (cdr (flet ((after (char)
                                 (member (find-directive (cdr directives)
                                                         char
                                                         nil)
                                         directives)))
                          (case char
                            (#\( (after #\)))
                            (#\< (after #\>))
                            (#\[ (after #\]))
                            (#\{ (after #\}))
                            (t directives))))
                   kind stop-at-semi)))
            (find-directive (cdr directives) kind stop-at-semi)))))

;;;; format directives for simple output

(def-format-directive #\A (colonp atsignp params)
  (if params
      (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                             (padchar #\space))
                     params
        `(format-princ stream ,(expand-next-arg) ',colonp ',atsignp
                       ,mincol ,colinc ,minpad ,padchar))
      `(princ ,(if colonp
                   `(or ,(expand-next-arg) "()")
                   (expand-next-arg))
              stream)))

(def-format-directive #\S (colonp atsignp params)
  (cond (params
         (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                (padchar #\space))
                        params
           `(format-prin1 stream ,(expand-next-arg) ,colonp ,atsignp
                          ,mincol ,colinc ,minpad ,padchar)))
        (colonp
         `(let ((arg ,(expand-next-arg)))
            (if arg
                (prin1 arg stream)
                (princ "()" stream))))
        (t
         `(prin1 ,(expand-next-arg) stream))))

(def-format-directive #\C (colonp atsignp params string end)
  (expand-bind-defaults () params
    (let ((n-arg (sb-xc:gensym "ARG")))
      `(let ((,n-arg ,(expand-next-arg)))
         (unless (typep ,n-arg 'character)
           (format-error-at ,string ,(1- end)
                            "~S is not of type CHARACTER." ,n-arg))
         ,(cond (colonp
                 `(format-print-named-character ,n-arg stream))
                (atsignp
                 `(prin1 ,n-arg stream))
                (t
                 `(write-char ,n-arg stream)))))))

(def-format-directive #\W (colonp atsignp params)
  (expand-bind-defaults () params
    (if (or colonp atsignp)
        `(let (,@(when colonp
                   '((*print-pretty* t)))
               ,@(when atsignp
                   '((*print-level* nil)
                     (*print-length* nil))))
           (output-object ,(expand-next-arg) stream))
        `(output-object ,(expand-next-arg) stream))))

;;;; format directives for integer output

(defun expand-format-integer (base colonp atsignp params)
  (if (or colonp atsignp params)
      (expand-bind-defaults
          ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
          params
        `(format-print-integer stream ,(expand-next-arg) ,colonp ,atsignp
                               ,base ,mincol ,padchar ,commachar
                               ,commainterval))
      `(let ((*print-base* ,base)
             (*print-radix* nil))
         (princ ,(expand-next-arg) stream))))

(def-format-directive #\D (colonp atsignp params)
  (expand-format-integer 10 colonp atsignp params))

(def-format-directive #\B (colonp atsignp params)
  (expand-format-integer 2 colonp atsignp params))

(def-format-directive #\O (colonp atsignp params)
  (expand-format-integer 8 colonp atsignp params))

(def-format-directive #\X (colonp atsignp params)
  (expand-format-integer 16 colonp atsignp params))

(def-format-directive #\R (colonp atsignp params string end)
  (expand-bind-defaults
      ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
       (commainterval 3))
      params
    (let ((n-arg (sb-xc:gensym "ARG")))
      `(let ((,n-arg ,(expand-next-arg)))
         (unless (or ,base
                     (integerp ,n-arg))
           (format-error-at ,string ,(1- end) "~S is not of type INTEGER." ,n-arg))
         (if ,base
             (format-print-integer stream ,n-arg ,colonp ,atsignp
                                   ,base ,mincol
                                   ,padchar ,commachar ,commainterval)
             ,(if atsignp
                  (if colonp
                      `(format-print-old-roman stream ,n-arg)
                      `(format-print-roman stream ,n-arg))
                  (if colonp
                      `(format-print-ordinal stream ,n-arg)
                      `(format-print-cardinal stream ,n-arg))))))))

;;;; format directive for pluralization

(def-format-directive #\P (colonp atsignp params end)
  (expand-bind-defaults () params
    (let ((arg (cond
                ((not colonp)
                 (expand-next-arg))
                (*orig-args-available*
                 `(if (eq orig-args args)
                      (format-error-at
                       ,*default-format-error-control-string* ,(1- end)
                       "No previous argument")
                      (do ((arg-ptr orig-args (cdr arg-ptr)))
                          ((eq (cdr arg-ptr) args)
                           (car arg-ptr)))))
                (*only-simple-args*
                 (unless *simple-args*
                   (format-error "No previous argument"))
                 (caar *simple-args*))
                (t
                 (/show0 "THROWing NEED-ORIG-ARGS from tilde-P")
                 (throw 'need-orig-args nil)))))
      (if atsignp
          `(write-string (if (eql ,arg 1) "y" "ies") stream)
          `(unless (eql ,arg 1) (write-char #\s stream))))))

;;;; format directives for floating point output

(def-format-directive #\F (colonp atsignp params)
  (check-modifier "colon" colonp)
  (expand-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space)) params
    `(format-fixed stream ,(expand-next-arg) ,w ,d ,k ,ovf ,pad ,atsignp)))

(def-format-directive #\E (colonp atsignp params)
  (check-modifier "colon" colonp)
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-exponential stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark
                         ,atsignp)))

(def-format-directive #\G (colonp atsignp params)
  (check-modifier "colon" colonp)
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-general stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp)))

(def-format-directive #\$ (colonp atsignp params)
  (expand-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    `(format-dollars stream ,(expand-next-arg) ,d ,n ,w ,pad ,colonp
                     ,atsignp)))

;;;; format directives for line/page breaks etc.

(def-format-directive #\% (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (cond ((not params)
         '(terpri stream))
        ((typep params '(cons (cons * (mod 65536)) null))
         `(write-string ,(make-string (cdar params) :initial-element #\Newline) stream))
        (t
         (expand-bind-defaults ((count 1)) params
           `(dotimes (i ,count)
              (terpri stream))))))

(def-format-directive #\& (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (if params
      (expand-bind-defaults ((count 1)) params
        `(progn
           (when (plusp ,count)
             (fresh-line stream)
             (dotimes (i (1- ,count))
               (terpri stream)))))
      '(fresh-line stream)))

(def-format-directive #\| (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (write-char (code-char form-feed-char-code) stream)))
      '(write-char (code-char form-feed-char-code) stream)))

(def-format-directive #\~ (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (write-char #\~ stream)))
      '(write-char #\~ stream)))

;;; We'll only get here when the directive usage is illegal.
;;; COMBINE-DIRECTIVES would have handled a legal directive.
(def-complex-format-directive #\newline (colonp atsignp params directives)
  (check-modifier '("colon" "at-sign") (and colonp atsignp))
  (values (expand-bind-defaults () params)
          (bug "Unreachable ~S" directives)))

;;;; format directives for tabs and simple pretty printing

(def-format-directive #\T (colonp atsignp params)
  (if colonp
      (expand-bind-defaults ((n 1) (m 1)) params
        `(pprint-tab ,(if atsignp :section-relative :section)
                     ,n ,m stream))
      (if atsignp
          (expand-bind-defaults ((colrel 1) (colinc 1)) params
            `(format-relative-tab stream ,colrel ,colinc))
          (expand-bind-defaults ((colnum 1) (colinc 1)) params
            `(format-absolute-tab stream ,colnum ,colinc)))))

(def-format-directive #\_ (colonp atsignp params)
  (expand-bind-defaults () params
    `(pprint-newline ,(if colonp
                          (if atsignp
                              :mandatory
                              :fill)
                          (if atsignp
                              :miser
                              :linear))
                     stream)))

(def-format-directive #\I (colonp atsignp params)
  (check-modifier "at-sign" atsignp)
  (expand-bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n stream)))

;;;; format directive for ~*

(def-format-directive #\* (colonp atsignp params end)
  (check-modifier '("colon" "at-sign") (and colonp atsignp))
  (flet ((make-lose (index)
           `(format-error-at
             ,*default-format-error-control-string* ,(1- end)
             "Index ~W is out of bounds. It should have been between ~
              0 and ~W."
             ,index (length orig-args))))
    (if atsignp
        (expand-bind-defaults ((posn 0)) params
          (unless *orig-args-available*
            (/show0 "THROWing NEED-ORIG-ARGS from tilde-@*")
            (throw 'need-orig-args nil))
          `(if (<= 0 ,posn (length orig-args))
               (setf args (nthcdr ,posn orig-args))
               ,(make-lose posn)))
        (if colonp
            (expand-bind-defaults ((n 1)) params
              (unless *orig-args-available*
                (/show0 "THROWing NEED-ORIG-ARGS from tilde-:*")
                (throw 'need-orig-args nil))
              `(do ((cur-posn 0 (1+ cur-posn))
                    (arg-ptr orig-args (cdr arg-ptr)))
                   ((eq arg-ptr args)
                    (let ((new-posn (- cur-posn ,n)))
                      (if (<= 0 new-posn (length orig-args))
                          (setf args (nthcdr new-posn orig-args))
                          ,(make-lose 'new-posn))))))
            (if params
                (expand-bind-defaults ((n 1)) params
                  (setf *only-simple-args* nil)
                  `(dotimes (i ,n)
                     ,(expand-next-arg)))
                (expand-next-arg))))))

;;;; format directive for indirection

(def-format-directive #\? (colonp atsignp params string end)
  (check-modifier "colon" colonp)
  (expand-bind-defaults () params
    `(handler-bind
         ((format-error
           (lambda (condition)
             (error 'format-error
                    :complaint
                    "~A~%while processing indirect format string:"
                    :args (list condition)
                    :print-banner nil
                    :control-string ,string
                    :offset ,(1- end)))))
       ,(if atsignp
            (if *orig-args-available*
                `(setf args (%format stream ,(expand-next-arg) orig-args args))
                (throw 'need-orig-args nil))
            `(%format stream ,(expand-next-arg) ,(expand-next-arg))))))

;;;; format directives for capitalization

(def-complex-format-directive #\( (colonp atsignp params directives)
  (let* ((close (or (find-directive directives #\) nil)
                    (format-error "No corresponding close parenthesis")))
         (posn (position close directives))
         (before (subseq directives 0 posn))
         (after (nthcdr (1+ posn) directives)))
    (values
     (expand-bind-defaults () params
       `(let ((stream (make-case-frob-stream stream
                                             ,(if colonp
                                                  (if atsignp
                                                      :upcase
                                                      :capitalize)
                                                  (if atsignp
                                                      :capitalize-first
                                                      :downcase)))))
          ,@(expand-directive-list before)))
     after)))

(def-complex-format-directive #\) ()
  (format-error "No corresponding open parenthesis"))

;;;; format directives and support functions for conditionalization

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (check-modifier '("colon" "at-sign") (and colonp atsignp))
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (values
     (cond
       (atsignp
        (when (cdr sublists)
          (format-error "Can only specify one section"))
        (expand-bind-defaults () params
          (expand-maybe-conditional (car sublists))))
       (colonp
        (unless (= (length sublists) 2)
          (format-error "Must specify exactly two sections"))
        (expand-bind-defaults () params
          (apply #'expand-true-false-conditional sublists)))
       (t
        (expand-bind-defaults ((index nil)) params
          (setf *only-simple-args* nil)
          (let ((clauses nil)
                (case-sym (gensym))
                (case `(or ,index ,(expand-next-arg))))
            (when last-semi-with-colon-p
              (push `(t ,@(expand-directive-list (pop sublists)))
                    clauses))
            (let ((count (length sublists)))
              (dolist (sublist sublists)
                (push `(,(decf count)
                         ,@(expand-directive-list sublist))
                      clauses)))
            `(let ((,case-sym ,case))
               (unless (integerp ,case-sym)
                 (format-error-at
                  ,*default-format-error-control-string*
                  ,*default-format-error-offset*
                  "The argument to ~~[ is not an integer: ~A" ,case-sym))
               (case ,case-sym ,@clauses))))))
     remaining)))

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
        (last-semi-with-colon-p nil)
        (remaining directives))
    (loop
       (let* ((close-or-semi (or (find-directive remaining #\] t)
                                 (format-error "No corresponding close bracket")))
              (posn (position close-or-semi remaining)))
         (push (subseq remaining 0 posn) sublists)
         (setf remaining (nthcdr (1+ posn) remaining))
         (when (char= (directive-character close-or-semi) #\])
           (return))
         (setf last-semi-with-colon-p
               (directive-colonp close-or-semi))))
    (values sublists last-semi-with-colon-p remaining)))

(defun expand-maybe-conditional (sublist)
  (flet ((hairy ()
           `(let ((prev-args args)
                  (arg ,(expand-next-arg)))
              (when arg
                (setf args prev-args)
                ,@(expand-directive-list sublist)))))
    (if *only-simple-args*
        (multiple-value-bind (guts new-args)
            (let ((*simple-args* *simple-args*))
              (values (expand-directive-list sublist)
                      *simple-args*))
          (cond ((and new-args (eq *simple-args* (cdr new-args)))
                 (setf *simple-args* new-args)
                 `(when ,(caar new-args)
                    ,@guts))
                (t
                 (setf *only-simple-args* nil)
                 (hairy))))
        (hairy))))

(defun expand-true-false-conditional (true false)
  (let ((arg (expand-next-arg)))
    (flet ((hairy ()
             `(if ,arg
                  (progn
                    ,@(expand-directive-list true))
                  (progn
                    ,@(expand-directive-list false)))))
      (if *only-simple-args*
          (multiple-value-bind (true-guts true-args true-simple)
              (let ((*simple-args* *simple-args*)
                    (*only-simple-args* t))
                (values (expand-directive-list true)
                        *simple-args*
                        *only-simple-args*))
            (multiple-value-bind (false-guts false-args false-simple)
                (let ((*simple-args* *simple-args*)
                      (*only-simple-args* t))
                  (values (expand-directive-list false)
                          *simple-args*
                          *only-simple-args*))
              (if (= (length true-args) (length false-args))
                  `(if ,arg
                       (progn
                         ,@true-guts)
                       ,(do ((false false-args (cdr false))
                             (true true-args (cdr true))
                             (bindings nil (cons `(,(caar false) ,(caar true))
                                                 bindings)))
                            ((eq true *simple-args*)
                             (setf *simple-args* true-args)
                             (setf *only-simple-args*
                                   (and true-simple false-simple))
                             (if bindings
                                 `(let ,bindings
                                    ,@false-guts)
                                 `(progn
                                    ,@false-guts)))))
                  (progn
                    (setf *only-simple-args* nil)
                    (hairy)))))
          (hairy)))))

(def-complex-format-directive #\; ()
  (format-error
   "~~; directive not contained within either ~~[...~~] or ~~<...~~>"))

(def-complex-format-directive #\] ()
  (format-error "No corresponding open bracket"))

;;;; format directive for up-and-out

(def-format-directive #\^ (colonp atsignp params)
  (check-modifier "at-sign" atsignp)
  (when (and colonp (not *up-up-and-out-allowed*))
    (format-error "Attempt to use ~~:^ outside a ~~:{...~~} construct"))
  `(when ,(expand-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
            `(cond (,arg3 (<= ,arg1 ,arg2 ,arg3))
                   (,arg2 (eql ,arg1 ,arg2))
                   (,arg1 (eql ,arg1 0))
                   (t ,(if colonp
                           '(null outside-args)
                           (progn
                             (setf *only-simple-args* nil)
                             '(null args))))))
     ,(if colonp
          '(return-from outside-loop nil)
          '(return))))

;;;; format directives for iteration

(def-complex-format-directive #\{ (colonp atsignp params string end directives)
  (let* ((close (or (find-directive directives #\} nil)
                    (format-error "No corresponding close brace")))
         (closed-with-colon (directive-colonp close))
         (posn (position close directives)))
    (labels
        ((compute-insides ()
           (if (zerop posn)
               (if *orig-args-available*
                   `((handler-bind
                         ((format-error
                           (lambda (condition)
                             (format-error-at*
                              ,string ,(1- end)
                              "~A~%while processing indirect format string:"
                              (list condition)
                              :print-banner nil))))
                       (setf args
                             (%format stream inside-string orig-args args))))
                   (throw 'need-orig-args nil))
               (let ((*up-up-and-out-allowed* colonp))
                 (expand-directive-list (subseq directives 0 posn)))))
         (compute-loop (count)
           (when atsignp
             (setf *only-simple-args* nil))
           `(loop
               ,@(unless closed-with-colon
                   '((when (null args)
                       (return))))
               ,@(when count
                   `((when (and ,count (minusp (decf ,count)))
                       (return))))
               ,@(if colonp
                     (let ((*expander-next-arg-macro* 'expander-next-arg)
                           (*only-simple-args* nil)
                           (*orig-args-available* t))
                       `((let* ((orig-args ,(expand-next-arg))
                                (outside-args args)
                                (args orig-args))
                           (declare (ignorable orig-args outside-args args))
                           (block nil
                             ,@(compute-insides)))))
                     (compute-insides))
               ,@(when closed-with-colon
                   '((when (null args)
                       (return))))))
         (compute-block (count)
           (if colonp
               `(block outside-loop
                  ,(compute-loop count))
               (compute-loop count)))
         (compute-bindings (count)
           (if atsignp
               (compute-block count)
               `(let* ((orig-args ,(expand-next-arg))
                       (args orig-args))
                  (declare (ignorable orig-args args))
                  ,(let ((*expander-next-arg-macro* 'expander-next-arg)
                         (*only-simple-args* nil)
                         (*orig-args-available* t))
                     (compute-block count))))))
      (values (if params
                  (expand-bind-defaults ((count nil)) params
                    (if (zerop posn)
                        `(let ((inside-string ,(expand-next-arg)))
                           ,(compute-bindings count))
                        (compute-bindings count)))
                  (if (zerop posn)
                      `(let ((inside-string ,(expand-next-arg)))
                         ,(compute-bindings nil))
                      (compute-bindings nil)))
              (nthcdr (1+ posn) directives)))))

(def-complex-format-directive #\} ()
  (format-error "No corresponding open brace"))

;;;; format directives and support functions for justification

(defconstant-eqx !illegal-inside-justification
  '#.(mapcar (lambda (x) (directive-bits (parse-directive x 0 nil)))
             '("~:>" "~:@>"
               "~:T" "~:@T"))
  #'equal)

;;; Reject ~W, ~_, ~I and certain other specific values of modifier+character.
(defun illegal-inside-justification-p (directive)
  (and (format-directive-p directive)
       (if (or (member (directive-bits directive) !illegal-inside-justification)
               (member (directive-character directive) '(#\W #\I #\_)))
           t
           nil)))

(def-complex-format-directive #\< (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
      (parse-format-justification directives)
    (values
     (if (directive-colonp close) ; logical block vs. justification
         (multiple-value-bind (prefix per-line-p insides suffix)
             (parse-format-logical-block segments colonp first-semi
                                         close params string end)
           (expand-format-logical-block prefix per-line-p insides
                                        suffix atsignp))
         (let ((count (reduce #'+ (mapcar (lambda (x)
                                            (count-if #'illegal-inside-justification-p x))
                                          segments))))
           (when (> count 0)
             ;; ANSI specifies that "an error is signalled" in this
             ;; situation.
             (format-error*
              "~D illegal directive~:P found inside justification block"
              (list count)
              :references '((:ansi-cl :section (22 3 5 2)))))
           ;; ANSI does not explicitly say that an error should be
           ;; signalled, but the @ modifier is not explicitly allowed
           ;; for ~> either.
           (when (directive-atsignp close)
             (format-error-at*
              nil (1- (directive-end close))
              "@ modifier not allowed in close directive of ~
               justification block (i.e. ~~<...~~@>."
              '()
              :references '((:ansi-cl :section (22 3 6 2)))))
           (expand-format-justification segments colonp atsignp
                                        first-semi params)))
     remaining)))

(def-complex-format-directive #\> ()
  (format-error "No corresponding open bracket"))

(defun parse-format-logical-block
       (segments colonp first-semi close params string end)
  (when params
    (format-error-at nil (caar params)
                     "No parameters can be supplied with ~~<...~~:>."))
  (multiple-value-bind (prefix insides suffix)
      (multiple-value-bind (prefix-default suffix-default)
          (if colonp (values "(" ")") (values "" ""))
        (flet ((extract-string (list prefix-p)
                 (let ((directive (find-if #'format-directive-p list)))
                   (if directive
                       (format-error-at*
                        nil (1- (directive-end directive))
                        "Cannot include format directives inside the ~
                         ~:[suffix~;prefix~] segment of ~~<...~~:>"
                        (list prefix-p)
                        :references '((:ansi-cl :section (22 3 5 2))))
                       (apply #'concatenate 'string list)))))
        (case (length segments)
          (0 (values prefix-default nil suffix-default))
          (1 (values prefix-default (car segments) suffix-default))
          (2 (values (extract-string (car segments) t)
                     (cadr segments) suffix-default))
          (3 (values (extract-string (car segments) t)
                     (cadr segments)
                     (extract-string (caddr segments) nil)))
          (t
           (format-error "Too many segments for ~~<...~~:>")))))
    (when (directive-atsignp close)
      (setf insides
            (add-fill-style-newlines insides
                                     string
                                     (if first-semi
                                         (directive-end first-semi)
                                         end))))
    (values prefix
            (and first-semi (directive-atsignp first-semi))
            insides
            suffix)))

(defun add-fill-style-newlines (list string offset &optional last-directive)
  (cond
    (list
     (let ((directive (car list)))
       (cond
         ((simple-string-p directive)
          (let* ((non-space (position #\Space directive :test #'char/=))
                 (newlinep (and last-directive
                                (char= (directive-character last-directive)
                                       #\Newline))))
            (cond
              ((and newlinep non-space)
               (nconc
                (list (subseq directive 0 non-space))
                (add-fill-style-newlines-aux
                 (subseq directive non-space) string (+ offset non-space))
                (add-fill-style-newlines
                 (cdr list) string (+ offset (length directive)))))
              (newlinep
               (cons directive
                     (add-fill-style-newlines
                      (cdr list) string (+ offset (length directive)))))
              (t
               (nconc (add-fill-style-newlines-aux directive string offset)
                      (add-fill-style-newlines
                       (cdr list) string (+ offset (length directive))))))))
         (t
          (cons directive
                (add-fill-style-newlines
                 (cdr list) string
                 (directive-end directive) directive))))))
    (t nil)))

(defun add-fill-style-newlines-aux (literal string offset)
  (let ((end (length literal))
        (posn 0))
    (collect ((results))
      (loop
        (let ((blank (position #\space literal :start posn)))
          (when (null blank)
            (results (subseq literal posn))
            (return))
          (let ((non-blank (or (position #\space literal :start blank
                                         :test #'char/=)
                               end)))
            (results (subseq literal posn non-blank))
            (results (make-format-directive
                      string (+ offset non-blank) (+ offset non-blank)
                      nil t nil #\_ nil)) ; params,colon,atsign,char,symbol
            (setf posn non-blank))
          (when (= posn end)
            (return))))
      (results))))

(defun parse-format-justification (directives)
  (let ((first-semi nil)
        (close nil)
        (remaining directives))
    (collect ((segments))
      (loop
         (let ((close-or-semi (or (find-directive remaining #\> t)
                                  (format-error "No corresponding close bracket"))))
           (let ((posn (position close-or-semi remaining)))
             (segments (subseq remaining 0 posn))
             (setf remaining (nthcdr (1+ posn) remaining)))
           (when (char= (directive-character close-or-semi) #\>)
             (setf close close-or-semi)
             (return))
           (unless first-semi
             (setf first-semi close-or-semi))))
      (values (segments) first-semi close remaining))))

(sb-xc:defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (format-error-at ,string ,offset "No more arguments"))
     (pprint-pop)
     (pop args)))

(defun expand-format-logical-block (prefix per-line-p insides suffix atsignp)
  `(let ((arg ,(if atsignp 'args (expand-next-arg))))
     ,@(when atsignp
         (setf *only-simple-args* nil)
         '((setf args nil)))
     (pprint-logical-block
         (stream arg
                 ,(if per-line-p :per-line-prefix :prefix) ,prefix
                 :suffix ,suffix)
       (let ((args arg)
             ,@(unless atsignp
                 `((orig-args arg))))
         (declare (ignorable args ,@(unless atsignp '(orig-args))))
         (block nil
           ,@(let ((*expander-next-arg-macro* 'expander-pprint-next-arg)
                   (*only-simple-args* nil)
                   (*orig-args-available*
                    (if atsignp *orig-args-available* t)))
               (expand-directive-list insides)))))))

(defun expand-format-justification (segments colonp atsignp first-semi params)
  (let ((newline-segment-p
         (and first-semi
              (directive-colonp first-semi))))
    (expand-bind-defaults
        ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
        params
      `(let ((segments nil)
             ,@(when newline-segment-p
                 '((newline-segment nil)
                   (extra-space 0)
                   (line-len 72))))
         (block nil
           ,@(when newline-segment-p
               `((setf newline-segment
                       (%with-output-to-string (stream)
                         ,@(expand-directive-list (pop segments))))
                 ,(expand-bind-defaults
                      ((extra 0)
                       (line-len '(or (sb-impl::line-length stream) 72)))
                      (directive-params first-semi)
                    `(setf extra-space ,extra line-len ,line-len))))
           ,@(mapcar (lambda (segment)
                       `(push (%with-output-to-string (stream)
                                ,@(expand-directive-list segment))
                              segments))
                     segments))
         (format-justification stream
                               ,@(if newline-segment-p
                                     '(newline-segment extra-space line-len)
                                     '(nil 0 0))
                               segments ,colonp ,atsignp
                               ,mincol ,colinc ,minpad ,padchar)))))

;;;; format directive and support function for user-defined method

(def-format-directive #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-fun-name string start end)))
    (collect ((param-names) (bindings))
      (dolist (param-and-offset params)
        (let ((param (cdr param-and-offset)))
          (let ((param-name (sb-xc:gensym "PARAM")))
            (param-names param-name)
            (bindings `(,param-name
                        ,(case param
                           (:arg (expand-next-arg))
                           (:remaining '(length args))
                           (t param)))))))
      `(let ,(bindings)
         (,symbol stream ,(expand-next-arg) ,colonp ,atsignp
                  ,@(param-names))))))

(defun extract-user-fun-name (string start end)
  ;; Searching backwards avoids finding the wrong slash in a funky string
  ;; such as "~'/,'//fun/" which passes #\/ twice to FUN as parameters.
  (let* ((slash (or (position #\/ string :start start :end (1- end)
                              :from-end t)
                    (format-error "Malformed ~~/ directive")))
         (name (nstring-upcase (subseq string (1+ slash) (1- end))))
         (first-colon (position #\: name))
         (second-colon (if first-colon (position #\: name :start (1+ first-colon))))
         (symbol
          (cond ((and second-colon (= second-colon (1+ first-colon)))
                 (subseq name (1+ second-colon)))
                (first-colon
                 (subseq name (1+ first-colon)))
                (t name)))
         (package
            (if (not first-colon)
                #.(find-package "COMMON-LISP-USER")
                (let ((package-name (subseq name 0 first-colon)))
                  (or (find-package package-name)
                      ;; FIXME: should be PACKAGE-ERROR? Could we just
                      ;; use FIND-UNDELETED-PACKAGE-OR-LOSE?
                      (format-error "No package named ~S" package-name))))))
    (intern symbol package)))

(defun extract-user-fun-directives (string)
  (let* ((tokens (handler-case
                     (combine-directives
                      (%tokenize-control-string string 0 (length string) nil)
                      nil)
                   (error (e)
                     (declare (ignore e))
                     (return-from extract-user-fun-directives (values nil nil)))))
         (max-len (loop for token in tokens
                        sum (if (format-directive-p token)
                                (- (directive-end token) (directive-start token))
                                (length token))))
         (new-string
          (make-array max-len :element-type 'character :fill-pointer 0))
         (symbols))
    (dolist (token tokens)
      (cond ((stringp token)
             (aver (not (find #\~ token)))
             (let ((new-start (fill-pointer new-string)))
               (incf (fill-pointer new-string) (length token))
               (replace new-string token :start1 new-start)))
            (t
             (let* ((start (directive-start token))
                    (end (directive-end token))
                    (len (- end start))
                    (new-start (fill-pointer new-string)))
               (cond ((eql (directive-character token) #\/)
                      (push (handler-case (extract-user-fun-name string start end)
                              (error (e)
                                (declare (ignore e))
                                (return-from extract-user-fun-directives
                                  (values nil nil))))
                            symbols)
                      ;; Don't copy past the first slash. Scan backwards for it
                      ;; exactly as is done in EXTRACT-USER-FUN-NAME above.
                      ;; This error can't really happen (as we've already produced
                      ;; a valid tokenization), but the error check avoids
                      ;; a warning about adding 1 to NIL.
                      (setq end (1+ (or (position #\/ string :start start :end (1- end)
                                                  :from-end t)
                                        (error "Malformed ~~/ directive"))))
                      ;; compute new length to copy, +1 is for trailing slash
                      (incf (fill-pointer new-string) (1+ (- end start)))
                      (setf (char new-string (1- (fill-pointer new-string))) #\/))
                     (t
                      (incf (fill-pointer new-string) len)))
               (replace new-string string :start1 new-start
                                          :start2 start :end2 end)))))
    (values (nreverse symbols)
            (possibly-base-stringize new-string))))

(push '("SB-FORMAT" tokens) *!removable-symbols*)
(sb-xc:defmacro tokens (string)
  (declare (string string))
  (multiple-value-bind (symbols new-string) (extract-user-fun-directives string)
    (if symbols
        (make-fmt-control-proxy new-string symbols)
        (possibly-base-stringize new-string))))

;;; compile-time checking for argument mismatch.  This code is
;;; inspired by that of Gerd Moellmann, and comes decorated with
;;; FIXMEs:
(defun %compiler-walk-format-string (string args)
  (let* ((string (coerce string 'simple-string))
         (*default-format-error-control-string* string))
    (macrolet ((incf-both (&optional (increment 1))
                 `(progn
                   (incf min ,increment)
                   (incf max ,increment)))
               (walk-complex-directive (function)
                 `(multiple-value-bind (min-inc max-inc remaining)
                   (,function directive directives args)
                   (incf min min-inc)
                   (incf max max-inc)
                   (setq directives remaining))))
      ;; FIXME: these functions take a list of arguments as well as
      ;; the directive stream.  This is to enable possibly some
      ;; limited type checking on FORMAT's arguments, as well as
      ;; simple argument count mismatch checking: when the minimum and
      ;; maximum argument counts are the same at a given point, we
      ;; know which argument is going to be used for a given
      ;; directive, and some (annotated below) require arguments of
      ;; particular types.
      (labels
          ((walk-justification (justification directives args)
             (declare (ignore args))
             (let ((*default-format-error-offset*
                    (1- (directive-end justification))))
               (multiple-value-bind (segments first-semi close remaining)
                   (parse-format-justification directives)
                 (declare (ignore segments first-semi))
                 (cond
                   ((not (directive-colonp close))
                    (values 0 0 directives))
                   ((directive-atsignp justification)
                    (values 0 call-arguments-limit directives))
                   ;; FIXME: here we could assert that the
                   ;; corresponding argument was a list.
                   (t (values 1 1 remaining))))))
           (walk-conditional (conditional directives args)
             (let ((*default-format-error-offset*
                    (1- (directive-end conditional))))
               (multiple-value-bind (sublists last-semi-with-colon-p remaining)
                   (parse-conditional-directive directives)
                 (declare (ignore last-semi-with-colon-p))
                 (let ((sub-max
                        (loop for s in sublists
                              maximize (nth-value
                                        1 (walk-directive-list s args)))))
                   (cond
                     ((directive-atsignp conditional)
                      (values 1 (max 1 sub-max) remaining))
                     ((loop for p in (directive-params conditional)
                            thereis (or (integerp (cdr p))
                                        (memq (cdr p) '(:remaining :arg))))
                      (values 0 sub-max remaining))
                     ;; FIXME: if not COLONP, then the next argument
                     ;; must be a number.
                     (t (values 1 (1+ sub-max) remaining)))))))
           (walk-iteration (iteration directives args)
             (declare (ignore args))
             (let ((*default-format-error-offset*
                    (1- (directive-end iteration))))
               (let* ((close (find-directive directives #\} nil))
                      (posn (or (position close directives)
                                (format-error "No corresponding close brace")))
                      (remaining (nthcdr (1+ posn) directives)))
                 ;; FIXME: if POSN is zero, the next argument must be
                 ;; a format control (either a function or a string).
                 (if (directive-atsignp iteration)
                     (values (if (zerop posn) 1 0)
                             call-arguments-limit
                             remaining)
                     ;; FIXME: the argument corresponding to this
                     ;; directive must be a list.
                     (let ((nreq (if (zerop posn) 2 1)))
                       (values nreq nreq remaining))))))
           (walk-directive-list (directives args)
             (let ((min 0) (max 0))
               (loop
                (let ((directive (pop directives)))
                  (when (null directive)
                    (return (values min (min max call-arguments-limit))))
                  (when (format-directive-p directive)
                    (incf-both (count :arg (directive-params directive)
                                      :key #'cdr))
                    (let ((c (directive-character directive)))
                      (cond
                        ((find c "ABCDEFGORSWX$/")
                         (incf-both))
                        ((char= c #\P)
                         (unless (directive-colonp directive)
                           (incf-both)))
                        ((or (find c "IT%&|_();>~") (char= c #\Newline)))
                        ;; FIXME: check correspondence of ~( and ~)
                        ((char= c #\<)
                         (walk-complex-directive walk-justification))
                        ((char= c #\[)
                         (walk-complex-directive walk-conditional))
                        ((char= c #\{)
                         (walk-complex-directive walk-iteration))
                        ((char= c #\?)
                         ;; FIXME: the argument corresponding to this
                         ;; directive must be a format control.
                         (cond
                           ((directive-atsignp directive)
                            (incf min)
                            (setq max call-arguments-limit))
                           (t (incf-both 2))))
                        (t (throw 'give-up-format-string-walk nil))))))))))
        (catch 'give-up-format-string-walk
          (let ((directives (tokenize-control-string string)))
            (walk-directive-list directives args)))))))

;;; Optimize common case of constant keyword arguments
;;; to WRITE and WRITE-TO-STRING
(flet
    ((expand (fn object keys)
      (do (streamvar bind ignore)
          ((or (atom keys) (atom (cdr keys)))
           (if keys ; fail
               (values nil t)
               (values
                (let* ((objvar (copy-symbol 'object))
                       (bind `((,objvar ,object) ,@(nreverse bind)))
                       (ignore (when ignore `((declare (ignore ,@ignore))))))
                  (case fn
                   (write
                    ;; When :STREAM was specified, this used to insert a call
                    ;; to (OUT-SYNONYM-OF STREAMVAR) which added junk to the
                    ;; expansion which was not likely to improve performance.
                    ;; The benefit of this transform is that it avoids runtime
                    ;; keyword parsing and binding of 16 specials vars, *not*
                    ;; that it can inline testing for T or NIL as the stream.
                    `(let ,bind ,@ignore
                       ,@(if streamvar
                            `((%write ,objvar ,streamvar))
                            `((output-object ,objvar *standard-output*)
                              ,objvar))))
                   (write-to-string
                    (if (cdr bind)
                        `(let ,bind ,@ignore (stringify-object ,objvar))
                        `(stringify-object ,object)))))
                nil)))
        (let* ((key (pop keys))
               (value (pop keys))
               (variable
                (cond ((getf '(:array *print-array*
                               :base *print-base*
                               :case *print-case*
                               :circle *print-circle*
                               :escape *print-escape*
                               :gensym *print-gensym*
                               :length *print-length*
                               :level *print-level*
                               :lines *print-lines*
                               :miser-width *print-miser-width*
                               :pprint-dispatch *print-pprint-dispatch*
                               :pretty *print-pretty*
                               :radix *print-radix*
                               :readably *print-readably*
                               :right-margin *print-right-margin*
                               :suppress-errors *suppress-print-errors*)
                             key))
                      ((and (eq key :stream) (eq fn 'write))
                       (or streamvar (setq streamvar (copy-symbol 'stream))))
                      (t
                       (return (values nil t))))))
          (when (assoc variable bind)
            ;; First key has precedence, but we still need to execute the
            ;; argument, and in the right order.
            (setf variable (gensym "IGNORE"))
            (push variable ignore))
          (push (list variable value) bind)))))

  (sb-c:define-source-transform write (object &rest keys)
    (expand 'write object keys))

  (sb-c:define-source-transform write-to-string (object &rest keys)
    (expand 'write-to-string object keys)))

#-sb-xc-host
(defun !late-format-init ()
  (setq sb-format::**tokenize-control-string-cache-vector** nil))
