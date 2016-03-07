;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FORMAT")

;;;; TOKENIZE-CONTROL-STRING

;;; The case for caching is to speed up out-of-line calls that use a fixed
;;; control string in a loop, not to avoid re-tokenizing all strings that
;;; happen to be STRING= to that string.
(defun-cached (tokenize-control-string
               :hash-bits 7
               :hash-function #+sb-xc-host
                              (lambda (x) (declare (ignore x)) 1)
                              #-sb-xc-host #'pointer-hash)
    ;; Due to string mutability, the comparator is STRING=
    ;; even though the hash is address-based.
    ((string string=))
  (declare (simple-string string))
  (let ((index 0)
        (end (length string))
        (result nil)
        ;; FIXME: consider rewriting this 22.3.5.2-related processing
        ;; using specials to maintain state and doing the logic inside
        ;; the directive expanders themselves.
        (block)
        (pprint)
        (semicolon)
        (justification-semicolon))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
        (when (> next-directive index)
          (push (subseq string index next-directive) result))
        (when (= next-directive end)
          (return))
        (let* ((directive (parse-directive string next-directive))
               (char (format-directive-character directive)))
          ;; this processing is required by CLHS 22.3.5.2
          (cond
            ((char= char #\<) (push directive block))
            ((and block (char= char #\;) (format-directive-colonp directive))
             (setf semicolon directive))
            ((char= char #\>)
             (unless block
               (error 'format-error
                      :complaint "~~> without a matching ~~<"
                      :control-string string
                      :offset next-directive))
             (cond
               ((format-directive-colonp directive)
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
               (#\T (when (and (format-directive-colonp directive)
                               (not pprint))
                      (setf pprint directive))))))
          (push directive result)
          (setf index (format-directive-end directive)))))
    (when (and pprint justification-semicolon)
      (let ((pprint-offset (1- (format-directive-end pprint)))
            (justification-offset
             (1- (format-directive-end justification-semicolon))))
        (error 'format-error
               :complaint "misuse of justification and pprint directives"
               :control-string string
               :offset (min pprint-offset justification-offset)
               :second-relative (- (max pprint-offset justification-offset)
                                   (min pprint-offset justification-offset)
                                   1)
               :references (list '(:ansi-cl :section (22 3 5 2))))))
    (nreverse result)))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
        (end (length string)))
    (flet ((get-char ()
             (if (= posn end)
                 (error 'format-error
                        :complaint "string ended before directive was found"
                        :control-string string
                        :offset start)
                 (schar string posn)))
           (check-ordering ()
             (when (or colonp atsignp)
               (error 'format-error
                      :complaint "parameters found after #\\: or #\\@ modifier"
                      :control-string string
                      :offset posn
                      :references (list '(:ansi-cl :section (22 3)))))))
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
                     (error 'format-error
                            :complaint "too many colons supplied"
                            :control-string string
                            :offset posn
                            :references (list '(:ansi-cl :section (22 3))))
                     (setf colonp t)))
                ((char= char #\@)
                 (if atsignp
                     (error 'format-error
                            :complaint "too many #\\@ characters supplied"
                            :control-string string
                            :offset posn
                            :references (list '(:ansi-cl :section (22 3))))
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
                (error 'format-error
                       :complaint "no matching closing slash"
                       :control-string string
                       :offset posn))))
        (make-format-directive
         :string string :start start :end (1+ posn)
         :character (char-upcase char)
         :colonp colonp :atsignp atsignp
         :params (nreverse params))))))

;;;; FORMATTER stuff

(sb!xc:defmacro formatter (control-string)
  `#',(%formatter control-string))


(defun %formatter (control-string &optional (arg-count 0) (need-retval t))
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
        (return `(lambda (stream ,@required
                                 ,@(if optional '(&optional)) ,@optional
                                 &rest args)
                   (declare (ignorable stream args))
                   ,guts
                   ,(and need-retval 'args)))))
    (let ((*orig-args-available* t)
          (*only-simple-args* nil))
      `(lambda (stream &rest orig-args)
         (declare (ignorable stream))
         (let ((args orig-args))
           ,(expand-control-string control-string)
           ,(and need-retval 'args))))))

(defun args-exhausted (control-string offset)
  (error 'format-error
         :complaint "required argument missing"
         :control-string control-string
         :offset offset))

(defun expand-control-string (string)
  (let* ((string (etypecase string
                   (simple-string
                    string)
                   (string
                    (coerce string 'simple-string))))
         (*default-format-error-control-string* string)
         (directives (tokenize-control-string string)))
    `(block nil
       ,@(expand-directive-list directives))))

(defun expand-directive-list (directives)
  (let ((results nil)
        (remaining-directives directives))
    (loop
      (unless remaining-directives
        (return))
      (multiple-value-bind (form new-directives)
          (expand-directive (car remaining-directives)
                            (cdr remaining-directives))
        (push form results)
        (setf remaining-directives new-directives)))
    (reverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
            (let ((char (format-directive-character directive)))
              (typecase char
                (base-char
                 (aref *format-directive-expanders* (sb!xc:char-code char))))))
           (*default-format-error-offset*
            (1- (format-directive-end directive))))
       (declare (type (or null function) expander))
       (if expander
           (funcall expander directive more-directives)
           (error 'format-error
                  :complaint "unknown directive ~@[(character: ~A)~]"
                  :args (list (char-name (format-directive-character directive)))))))
    (simple-string
     (values `(write-string ,directive stream)
             more-directives))))

(sb!xc:defmacro expander-next-arg (string offset)
  `(if args
       (pop args)
       (error 'format-error
              :complaint "no more arguments"
              :control-string ,string
              :offset ,offset)))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(,*expander-next-arg-macro*
        ,*default-format-error-control-string*
        ,(or offset *default-format-error-offset*))
      (let ((symbol (sb!xc:gensym "FORMAT-ARG")))
        (push (cons symbol (or offset *default-format-error-offset*))
              *simple-args*)
        symbol)))

(defmacro expand-bind-defaults (specs params &body body)
  (once-only ((params params))
    (if specs
        (collect ((expander-bindings) (runtime-bindings))
          (dolist (spec specs)
            (destructuring-bind (var default) spec
              (let ((symbol (sb!xc:gensym "FVAR")))
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
                    (error
                     'format-error
                     :complaint "too many parameters, expected no more than ~W"
                     :args (list ,(length specs))
                     :offset (caar ,params)))
              ,,@body)))
        `(progn
           (when ,params
             (error 'format-error
                    :complaint "too many parameters, expected none"
                    :offset (caar ,params)))
           ,@body))))

;;;; format directive machinery

(eval-when (:compile-toplevel :execute)
(#+sb-xc-host defmacro #-sb-xc-host sb!xc:defmacro def-complex-format-directive (char lambda-list &body body)
  (let ((defun-name (intern (format nil
                                    "~:@(~:C~)-FORMAT-DIRECTIVE-EXPANDER"
                                    char)))
        (directive (sb!xc:gensym "DIRECTIVE"))
        (directives (if lambda-list (car (last lambda-list)) (sb!xc:gensym "DIRECTIVES"))))
    `(progn
       (defun ,defun-name (,directive ,directives)
         ,@(if lambda-list
               `((let ,(mapcar (lambda (var)
                                 `(,var
                                   (,(symbolicate "FORMAT-DIRECTIVE-" var)
                                    ,directive)))
                               (butlast lambda-list))
                   ,@body))
               `((declare (ignore ,directive ,directives))
                 ,@body)))
       (%set-format-directive-expander ,char #',defun-name))))

(#+sb-xc-host defmacro #-sb-xc-host sb!xc:defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (sb!xc:gensym "DIRECTIVES"))
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
) ; EVAL-WHEN

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

(defun %set-format-directive-expander (char fn)
  (let ((code (sb!xc:char-code (char-upcase char))))
    (setf (aref *format-directive-expanders* code) fn))
  char)

(defun %set-format-directive-interpreter (char fn)
  (let ((code (sb!xc:char-code (char-upcase char))))
    (setf (aref *format-directive-interpreters* code) fn))
  char)

(defun find-directive (directives kind stop-at-semi)
  (if directives
      (let ((next (car directives)))
        (if (format-directive-p next)
            (let ((char (format-directive-character next)))
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

) ; EVAL-WHEN

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
    (let ((n-arg (sb!xc:gensym "ARG")))
      `(let ((,n-arg ,(expand-next-arg)))
         (unless (typep ,n-arg 'character)
           (error 'format-error
                  :complaint "~s is not of type CHARACTER."
                  :args (list ,n-arg)
                  :control-string ,string
                  :offset ,(1- end)))
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
    (let ((n-arg (sb!xc:gensym "ARG")))
      `(let ((,n-arg ,(expand-next-arg)))
         (unless (or ,base
                     (integerp ,n-arg))
           (error 'format-error
                  :complaint "~s is not of type INTEGER."
                  :args (list ,n-arg)
                  :control-string ,string
                  :offset ,(1- end)))
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
                      (error 'format-error
                             :complaint "no previous argument"
                             :offset ,(1- end))
                      (do ((arg-ptr orig-args (cdr arg-ptr)))
                          ((eq (cdr arg-ptr) args)
                           (car arg-ptr)))))
                (*only-simple-args*
                 (unless *simple-args*
                   (error 'format-error
                          :complaint "no previous argument"))
                 (caar *simple-args*))
                (t
                 (/show0 "THROWing NEED-ORIG-ARGS from tilde-P")
                 (throw 'need-orig-args nil)))))
      (if atsignp
          `(write-string (if (eql ,arg 1) "y" "ies") stream)
          `(unless (eql ,arg 1) (write-char #\s stream))))))

;;;; format directives for floating point output

(def-format-directive #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space)) params
    `(format-fixed stream ,(expand-next-arg) ,w ,d ,k ,ovf ,pad ,atsignp)))

(def-format-directive #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-exponential stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark
                         ,atsignp)))

(def-format-directive #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "The colon modifier cannot be used with this directive."))
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
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "The colon and atsign modifiers cannot be used with this directive."
           ))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (terpri stream)))
      '(terpri stream)))

(def-format-directive #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "The colon and atsign modifiers cannot be used with this directive."
           ))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(progn
           (when (plusp ,count)
             (fresh-line stream)
             (dotimes (i (1- ,count))
               (terpri stream)))))
      '(fresh-line stream)))

(def-format-directive #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "The colon and atsign modifiers cannot be used with this directive."
           ))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (write-char (code-char form-feed-char-code) stream)))
      '(write-char (code-char form-feed-char-code) stream)))

(def-format-directive #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "The colon and atsign modifiers cannot be used with this directive."
           ))
  (if params
      (expand-bind-defaults ((count 1)) params
        `(dotimes (i ,count)
           (write-char #\~ stream)))
      '(write-char #\~ stream)))

(def-complex-format-directive #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    ;; FIXME: this is not an error!
    (error 'format-error
           :complaint "both colon and atsign modifiers used simultaneously"))
  (values (expand-bind-defaults () params
            (if atsignp
                '(write-char #\newline stream)
                nil))
          (if (and (not colonp)
                   directives
                   (simple-string-p (car directives)))
              (cons (string-left-trim *format-whitespace-chars*
                                      (car directives))
                    (cdr directives))
              directives)))

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
  (when atsignp
    (error 'format-error
           :complaint
           "cannot use the at-sign modifier with this directive"))
  (expand-bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n stream)))

;;;; format directive for ~*

(def-format-directive #\* (colonp atsignp params end)
  (if atsignp
      (if colonp
          (error 'format-error
                 :complaint
                 "both colon and atsign modifiers used simultaneously")
          (expand-bind-defaults ((posn 0)) params
            (unless *orig-args-available*
              (/show0 "THROWing NEED-ORIG-ARGS from tilde-@*")
              (throw 'need-orig-args nil))
            `(if (<= 0 ,posn (length orig-args))
                 (setf args (nthcdr ,posn orig-args))
                 (error 'format-error
                        :complaint "Index ~W out of bounds. Should have been ~
                                    between 0 and ~W."
                        :args (list ,posn (length orig-args))
                        :offset ,(1- end)))))
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
                        (error 'format-error
                               :complaint
                               "Index ~W is out of bounds; should have been ~
                                between 0 and ~W."
                               :args (list new-posn (length orig-args))
                               :offset ,(1- end)))))))
          (if params
              (expand-bind-defaults ((n 1)) params
                (setf *only-simple-args* nil)
                `(dotimes (i ,n)
                   ,(expand-next-arg)))
              (expand-next-arg)))))

;;;; format directive for indirection

(def-format-directive #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
           :complaint "cannot use the colon modifier with this directive"))
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
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
             :complaint "no corresponding close parenthesis"))
    (let* ((posn (position close directives))
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
       after))))

(def-complex-format-directive #\) ()
  (error 'format-error
         :complaint "no corresponding open parenthesis"))

;;;; format directives and support functions for conditionalization

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (values
     (if atsignp
         (if colonp
             (error 'format-error
                    :complaint
                    "both colon and atsign modifiers used simultaneously")
             (if (cdr sublists)
                 (error 'format-error
                        :complaint
                        "Can only specify one section")
                 (expand-bind-defaults () params
                   (expand-maybe-conditional (car sublists)))))
         (if colonp
             (if (= (length sublists) 2)
                 (expand-bind-defaults () params
                   (expand-true-false-conditional (car sublists)
                                                  (cadr sublists)))
                 (error 'format-error
                        :complaint
                        "must specify exactly two sections"))
             (expand-bind-defaults ((index nil)) params
               (setf *only-simple-args* nil)
               (let ((clauses nil)
                     (case `(or ,index ,(expand-next-arg))))
                 (when last-semi-with-colon-p
                   (push `(t ,@(expand-directive-list (pop sublists)))
                         clauses))
                 (let ((count (length sublists)))
                   (dolist (sublist sublists)
                     (push `(,(decf count)
                             ,@(expand-directive-list sublist))
                           clauses)))
                 `(case ,case ,@clauses)))))
     remaining)))

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
        (last-semi-with-colon-p nil)
        (remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
        (unless close-or-semi
          (error 'format-error
                 :complaint "no corresponding close bracket"))
        (let ((posn (position close-or-semi remaining)))
          (push (subseq remaining 0 posn) sublists)
          (setf remaining (nthcdr (1+ posn) remaining))
          (when (char= (format-directive-character close-or-semi) #\])
            (return))
          (setf last-semi-with-colon-p
                (format-directive-colonp close-or-semi)))))
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
  (error 'format-error
         :complaint
         "~~; directive not contained within either ~~[...~~] or ~~<...~~>"))

(def-complex-format-directive #\] ()
  (error 'format-error
         :complaint
         "no corresponding open bracket"))

;;;; format directive for up-and-out

(def-format-directive #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
           :complaint "cannot use the at-sign modifier with this directive"))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
           :complaint "attempt to use ~~:^ outside a ~~:{...~~} construct"))
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
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
             :complaint "no corresponding close brace"))
    (let* ((closed-with-colon (format-directive-colonp close))
           (posn (position close directives)))
      (labels
          ((compute-insides ()
             (if (zerop posn)
                 (if *orig-args-available*
                     `((handler-bind
                           ((format-error
                             (lambda (condition)
                               (error 'format-error
                                      :complaint
                              "~A~%while processing indirect format string:"
                                      :args (list condition)
                                      :print-banner nil
                                      :control-string ,string
                                      :offset ,(1- end)))))
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
                (nthcdr (1+ posn) directives))))))

(def-complex-format-directive #\} ()
  (error 'format-error
         :complaint "no corresponding open brace"))

;;;; format directives and support functions for justification

(defparameter *illegal-inside-justification*
  (mapcar (lambda (x) (parse-directive x 0))
          '("~W" "~:W" "~@W" "~:@W"
            "~_" "~:_" "~@_" "~:@_"
            "~:>" "~:@>"
            "~I" "~:I" "~@I" "~:@I"
            "~:T" "~:@T")))

(defun illegal-inside-justification-p (directive)
  (member directive *illegal-inside-justification*
          :test (lambda (x y)
                  (and (format-directive-p x)
                       (format-directive-p y)
                       (eql (format-directive-character x) (format-directive-character y))
                       (eql (format-directive-colonp x) (format-directive-colonp y))
                       (eql (format-directive-atsignp x) (format-directive-atsignp y))))))

(def-complex-format-directive #\< (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
      (parse-format-justification directives)
    (values
     (if (format-directive-colonp close) ; logical block vs. justification
         (multiple-value-bind (prefix per-line-p insides suffix)
             (parse-format-logical-block segments colonp first-semi
                                         close params string end)
           (expand-format-logical-block prefix per-line-p insides
                                        suffix atsignp))
         (let ((count (reduce #'+ (mapcar (lambda (x) (count-if #'illegal-inside-justification-p x)) segments))))
           (when (> count 0)
             ;; ANSI specifies that "an error is signalled" in this
             ;; situation.
             (error 'format-error
                    :complaint "~D illegal directive~:P found inside justification block"
                    :args (list count)
                    :references (list '(:ansi-cl :section (22 3 5 2)))))
           ;; ANSI does not explicitly say that an error should be
           ;; signalled, but the @ modifier is not explicitly allowed
           ;; for ~> either.
           (when (format-directive-atsignp close)
             (error 'format-error
                    :complaint "@ modifier not allowed in close ~
                    directive of justification ~
                    block (i.e. ~~<...~~@>."
                    :offset (1- (format-directive-end close))
                    :references (list '(:ansi-cl :section (22 3 6 2)))))
           (expand-format-justification segments colonp atsignp
                                        first-semi params)))
     remaining)))

(def-complex-format-directive #\> ()
  (error 'format-error
         :complaint "no corresponding open bracket"))

(defun parse-format-logical-block
       (segments colonp first-semi close params string end)
  (when params
    (error 'format-error
           :complaint "No parameters can be supplied with ~~<...~~:>."
           :offset (caar params)))
  (multiple-value-bind (prefix insides suffix)
      (multiple-value-bind (prefix-default suffix-default)
          (if colonp (values "(" ")") (values "" ""))
        (flet ((extract-string (list prefix-p)
                 (let ((directive (find-if #'format-directive-p list)))
                   (if directive
                       (error 'format-error
                              :complaint
                              "cannot include format directives inside the ~
                               ~:[suffix~;prefix~] segment of ~~<...~~:>"
                              :args (list prefix-p)
                              :offset (1- (format-directive-end directive))
                              :references
                              (list '(:ansi-cl :section (22 3 5 2))))
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
           (error 'format-error
                  :complaint "too many segments for ~~<...~~:>")))))
    (when (format-directive-atsignp close)
      (setf insides
            (add-fill-style-newlines insides
                                     string
                                     (if first-semi
                                         (format-directive-end first-semi)
                                         end))))
    (values prefix
            (and first-semi (format-directive-atsignp first-semi))
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
                                (char=
                                 (format-directive-character last-directive)
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
                 (format-directive-end directive) directive))))))
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
                      :string string :character #\_
                      :start (+ offset non-blank) :end (+ offset non-blank)
                      :colonp t :atsignp nil :params nil))
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
        (let ((close-or-semi (find-directive remaining #\> t)))
          (unless close-or-semi
            (error 'format-error
                   :complaint "no corresponding close bracket"))
          (let ((posn (position close-or-semi remaining)))
            (segments (subseq remaining 0 posn))
            (setf remaining (nthcdr (1+ posn) remaining)))
          (when (char= (format-directive-character close-or-semi)
                       #\>)
            (setf close close-or-semi)
            (return))
          (unless first-semi
            (setf first-semi close-or-semi))))
      (values (segments) first-semi close remaining))))

(sb!xc:defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error
              :complaint "no more arguments"
              :control-string ,string
              :offset ,offset))
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
              (format-directive-colonp first-semi))))
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
                       (with-simple-output-to-string (stream)
                         ,@(expand-directive-list (pop segments))))
                 ,(expand-bind-defaults
                      ((extra 0)
                       (line-len '(or (sb!impl::line-length stream) 72)))
                      (format-directive-params first-semi)
                    `(setf extra-space ,extra line-len ,line-len))))
           ,@(mapcar (lambda (segment)
                       `(push (with-simple-output-to-string (stream)
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
          (let ((param-name (sb!xc:gensym "PARAM")))
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
  (let ((slash (position #\/ string :start start :end (1- end)
                         :from-end t)))
    (unless slash
      (error 'format-error
             :complaint "malformed ~~/ directive"))
    (let* ((name (string-upcase (let ((foo string))
                                  ;; Hack alert: This is to keep the compiler
                                  ;; quiet about deleting code inside the
                                  ;; subseq expansion.
                                  (subseq foo (1+ slash) (1- end)))))
           (first-colon (position #\: name))
           (second-colon (if first-colon (position #\: name :start (1+ first-colon))))
           (package-name (if first-colon
                             (subseq name 0 first-colon)
                             "COMMON-LISP-USER"))
           (package (find-package package-name)))
      (unless package
        ;; FIXME: should be PACKAGE-ERROR? Could we just use
        ;; FIND-UNDELETED-PACKAGE-OR-LOSE?
        (error 'format-error
               :complaint "no package named ~S"
               :args (list package-name)))
      (intern (cond
                ((and second-colon (= second-colon (1+ first-colon)))
                 (subseq name (1+ second-colon)))
                (first-colon
                 (subseq name (1+ first-colon)))
                (t name))
              package))))

;;; compile-time checking for argument mismatch.  This code is
;;; inspired by that of Gerd Moellmann, and comes decorated with
;;; FIXMEs:
(defun %compiler-walk-format-string (string args)
  (declare (type simple-string string))
  (let ((*default-format-error-control-string* string))
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
                    (1- (format-directive-end justification))))
               (multiple-value-bind (segments first-semi close remaining)
                   (parse-format-justification directives)
                 (declare (ignore segments first-semi))
                 (cond
                   ((not (format-directive-colonp close))
                    (values 0 0 directives))
                   ((format-directive-atsignp justification)
                    (values 0 sb!xc:call-arguments-limit directives))
                   ;; FIXME: here we could assert that the
                   ;; corresponding argument was a list.
                   (t (values 1 1 remaining))))))
           (walk-conditional (conditional directives args)
             (let ((*default-format-error-offset*
                    (1- (format-directive-end conditional))))
               (multiple-value-bind (sublists last-semi-with-colon-p remaining)
                   (parse-conditional-directive directives)
                 (declare (ignore last-semi-with-colon-p))
                 (let ((sub-max
                        (loop for s in sublists
                              maximize (nth-value
                                        1 (walk-directive-list s args)))))
                   (cond
                     ((format-directive-atsignp conditional)
                      (values 1 (max 1 sub-max) remaining))
                     ((loop for p in (format-directive-params conditional)
                            thereis (or (integerp (cdr p))
                                        (memq (cdr p) '(:remaining :arg))))
                      (values 0 sub-max remaining))
                     ;; FIXME: if not COLONP, then the next argument
                     ;; must be a number.
                     (t (values 1 (1+ sub-max) remaining)))))))
           (walk-iteration (iteration directives args)
             (declare (ignore args))
             (let ((*default-format-error-offset*
                    (1- (format-directive-end iteration))))
               (let* ((close (find-directive directives #\} nil))
                      (posn (or (position close directives)
                                (error 'format-error
                                       :complaint "no corresponding close brace")))
                      (remaining (nthcdr (1+ posn) directives)))
                 ;; FIXME: if POSN is zero, the next argument must be
                 ;; a format control (either a function or a string).
                 (if (format-directive-atsignp iteration)
                     (values (if (zerop posn) 1 0)
                             sb!xc:call-arguments-limit
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
                    (return (values min (min max sb!xc:call-arguments-limit))))
                  (when (format-directive-p directive)
                    (incf-both (count :arg (format-directive-params directive)
                                      :key #'cdr))
                    (let ((c (format-directive-character directive)))
                      (cond
                        ((find c "ABCDEFGORSWX$/")
                         (incf-both))
                        ((char= c #\P)
                         (unless (format-directive-colonp directive)
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
                           ((format-directive-atsignp directive)
                            (incf min)
                            (setq max sb!xc:call-arguments-limit))
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

  (sb!c:define-source-transform write (object &rest keys)
    (expand 'write object keys))

  (sb!c:define-source-transform write-to-string (object &rest keys)
    (expand 'write-to-string object keys)))
