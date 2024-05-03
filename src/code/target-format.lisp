;;;; functions to implement FORMAT and FORMATTER

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-FORMAT")

;;;; FORMAT

;;; This funcallable instance is used only as plain-old-data (conveying
;;; just its slots, not the funcallable nature). Being a function, it
;;; satisfies the type for the format-control argument of FORMAT, ERROR etc.
;;; Also note that (DEFTYPE FORMAT-CONTROL) = (OR STRING FUNCTION).
;;; And it's possible that we could decide to install a closure as
;;; the fin-fun but I don't think that's necessary.
(sb-kernel:!defstruct-with-alternate-metaclass fmt-control
  :slot-names ((string simple-string) symbols memo)
  :constructor %make-fmt-control
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)

(defmethod print-object ((self fmt-control) stream)
  (print-unreadable-object (self stream :type t)
    (write-string (unparse-fmt-control self) stream)))

(defmethod print-object ((x format-directive) stream)
  (print-unreadable-object (x stream)
    (let ((fun (directive-function x)))
      (write-string (directive-string x)
                    stream
                    :start (directive-start x)
                    :end (- (directive-end x) (if fun 1 0)))
      (when fun
        (print-symbol-with-prefix stream fun)
        (write-char #\/ stream)))))

(defun dummy (&rest args) (error "Should not be called: ~S~%" args))

(defun make-fmt-control (string symbols)
  (let ((f (%make-fmt-control string symbols nil)))
    (setf (%funcallable-instance-fun f) #'dummy)
    f))

(defun unparse-fmt-control (fmt)
  (%with-output-to-string (s)
    (write-char #\" s)
    (let ((symbols (fmt-control-symbols fmt)))
      (dolist (piece (tokenize-control-string (fmt-control-string fmt)))
        (cond ((stringp piece)
               (if (find #\Newline piece)
                   (dovector (c piece)
                     (if (char= c #\newline) (write-string "~%" s) (write-char c s)))
                   (write-string piece s)))
              (t
               (let* ((userfun (eql (directive-character piece) #\/))
                      (end (- (directive-end piece) (if userfun 1 0))))
                 (write-string (directive-string piece)
                               s
                               :start (directive-start piece)
                               :end end)
                 (when userfun
                   (print-symbol-with-prefix s (pop symbols))
                   (write-char #\/ s)))))))
    (write-char #\" s)))

(defun format (destination control-string &rest format-arguments)
  "Provides various facilities for formatting output.
  CONTROL-STRING contains a string to be output, possibly with embedded
  directives, which are flagged with the escape character \"~\". Directives
  generally expand into additional text to be output, usually consuming one
  or more of the FORMAT-ARGUMENTS in the process. A few useful directives
  are:
        ~A or ~nA   Prints one argument as if by PRINC
        ~S or ~nS   Prints one argument as if by PRIN1
        ~D or ~nD   Prints one argument as a decimal integer
        ~%          Does a TERPRI
        ~&          Does a FRESH-LINE
  where n is the width of the field in which the object is printed.

  DESTINATION controls where the result will go. If DESTINATION is T, then
  the output is sent to the standard output stream. If it is NIL, then the
  output is returned in a string as the value of the call. Otherwise,
  DESTINATION must be a stream to which the output will be sent.

  Example:   (FORMAT NIL \"The answer is ~D.\" 10) => \"The answer is 10.\"

  FORMAT has many additional capabilities not described here. Consult the
  manual for details."
  (declare (explicit-check)
           (dynamic-extent format-arguments))
  (etypecase destination
    (null
     (%with-output-to-string (stream)
       (%format stream control-string format-arguments)))
    (string
     (with-output-to-string (stream destination)
       (%format stream control-string format-arguments))
     nil)
    ((member t)
     (%format *standard-output* control-string format-arguments)
     nil)
    (stream
     (%format destination control-string format-arguments)
     nil)))

(defun %format (stream string-or-fun orig-args &optional (args orig-args))
  (if (and (functionp string-or-fun) (not (typep string-or-fun 'fmt-control)))
      (apply string-or-fun stream args)
      (truly-the
       (values t &optional)
       (catch 'up-and-out
         (let* ((string (etypecase string-or-fun
                          (simple-string
                           string-or-fun)
                          (string
                           (coerce string-or-fun 'simple-string))
                          ;; Not just more compact than testing for fmt-control
                          ;; but also produces a better error message.
                          (function
                           (fmt-control-string string-or-fun))))
                (*default-format-error-control-string* string)
                (*logical-block-popper* nil)
                (tokens
                  (if (functionp string-or-fun)
                      (or (fmt-control-memo string-or-fun)
                          ;; Memoize the parse back into the object
                          (setf (fmt-control-memo string-or-fun)
                                (%tokenize-control-string
                                 string 0 (length string)
                                 (fmt-control-symbols string-or-fun))))
                      (tokenize-control-string string))))
           (interpret-directive-list stream tokens orig-args args))))))

(!begin-collecting-cold-init-forms)
(define-load-time-global *format-directive-interpreters* nil)
(!cold-init-forms
 (setq *format-directive-interpreters* (make-array 128 :initial-element nil)))
(declaim (type (simple-vector 128)
               *format-directive-interpreters*))

(defun interpret-directive-list (stream directives orig-args args)
  (loop
   (unless directives
     (return args))
   (let ((directive (car directives)))
     (etypecase directive
      (simple-string
       (pop directives)
       (write-string directive stream))
      (format-directive
       (let ((function (svref *format-directive-interpreters*
                              (directive-code directive))))
         (multiple-value-setq
          (directives args)
          (let ((*default-format-error-offset*
                 (1- (directive-end directive))))
            (if (functionp function)
                (funcall function stream directive
                         (cdr directives) orig-args args)
                (format-error "Unknown format directive ~@[(character: ~A)~]"
                              (directive-char-name directive)))))))))))

;;;; FORMAT directive definition macros and runtime support

;;; This macro is used to extract the next argument from the current arg list.
;;; This is the version used by format directive interpreters.
(defmacro next-arg (&optional offset)
  `(progn
     (when (null args)
       (,@(if offset
              `(format-error-at nil ,offset)
              '(format-error))
          "No more arguments"))
     (when *logical-block-popper*
       (funcall *logical-block-popper*))
     (pop args)))

(defmacro def-complex-format-interpreter (char lambda-list &body body)
  (aver (not (lower-case-p char)))
  (let ((defun-name (directive-handler-name char "-INTERPRETER"))
        (directive '.directive) ; expose this var to the lambda. it's easiest
        (directives (if lambda-list (car (last lambda-list)) (gensym "DIRECTIVES"))))
    `(progn
       (defun ,defun-name (stream ,directive ,directives orig-args args)
         (declare (ignorable stream orig-args args))
         ,@(if lambda-list
               `((let ,(mapcar (lambda (var)
                                 `(,var
                                   (,(symbolicate "DIRECTIVE-" var) ,directive)))
                               (butlast lambda-list))
                   (values (progn ,@body) args)))
               `((declare (ignore ,directive ,directives))
                 ,@body)))
       (!cold-init-forms
        (setf (aref *format-directive-interpreters* (char-code ,char))
              #',defun-name)))))

(defmacro def-format-interpreter (char lambda-list &body body)
  (let ((directives (gensym "DIRECTIVES")))
    `(def-complex-format-interpreter ,char (,@lambda-list ,directives)
       ,@body
       ,directives)))

(defmacro interpret-bind-defaults (specs params &body body)
  (once-only ((params params))
    (collect ((bindings))
      (dolist (spec specs)
        (destructuring-bind (var default) spec
          (bindings `(,var (let* ((param-and-offset (pop ,params))
                                  (offset (car param-and-offset))
                                  (param (cdr param-and-offset)))
                             (case param
                               (:arg (or (next-arg offset) ,default))
                               (:remaining (length args))
                               ((nil) ,default)
                               (t param)))))))
      `(let* ,(bindings)
         (when ,params
           (format-error-at
            nil (caar ,params)
            "Too many parameters, expected no more than ~W" ,(length specs)))
         ,@body))))

;;;; format interpreters and support functions for simple output

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (when (and colinc (<= colinc 0))
    (format-error "The value of colinc is ~A, should be a positive integer"
                  colinc))
  (when (and mincol (< mincol 0))
    (format-error "The value of mincol is ~A, should be a non-negative integer"
                  mincol))
  (unless padleft
    (write-string string stream))
  (dotimes (i minpad)
    (write-char padchar stream))
  ;; As of sbcl-0.6.12.34, we could end up here when someone tries to
  ;; print e.g. (FORMAT T "~F" "NOTFLOAT"), in which case ANSI says
  ;; we're supposed to soldier on bravely, and so we have to deal with
  ;; the unsupplied-MINCOL-and-COLINC case without blowing up.
  (when (and mincol colinc)
    (do ((chars (+ (length string) (max minpad 0)) (+ chars colinc)))
        ((>= chars mincol))
      (dotimes (i colinc)
        (write-char padchar stream))))
  (when padleft
    (write-string string stream)))

(defun format-princ (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp))
                          (princ-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

(def-format-interpreter #\A (colonp atsignp params)
  (if params
      (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                (padchar #\space))
                     params
        (format-princ stream (next-arg) colonp atsignp
                      mincol colinc minpad padchar))
      (princ (if colonp (or (next-arg) "()") (next-arg)) stream)))

(defun format-prin1 (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp))
                          (prin1-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

(def-format-interpreter #\S (colonp atsignp params)
  (cond (params
         (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                   (padchar #\space))
                        params
           (format-prin1 stream (next-arg) colonp atsignp
                         mincol colinc minpad padchar)))
        (colonp
         (let ((arg (next-arg)))
           (if arg
               (prin1 arg stream)
               (princ "()" stream))))
        (t
         (prin1 (next-arg) stream))))

(def-format-interpreter #\C (colonp atsignp params)
  (interpret-bind-defaults () params
    (let ((arg (next-arg)))
      (unless (typep arg 'character)
        (format-error "~S is not of type CHARACTER." arg))
      (cond (colonp
             (format-print-named-character arg stream))
            (atsignp
             (prin1 arg stream))
            (t
             (write-char arg stream))))))

;;; "printing" as defined in the ANSI CL glossary, which is normative.
(defun char-printing-p (char)
  (and (not (eql char #\Space))
       (graphic-char-p char)))

(defun format-print-named-character (char stream)
  (cond ((not (char-printing-p char))
         (write-string (string-capitalize (char-name char)) stream))
        (t
         (write-char char stream))))

(def-format-interpreter #\W (colonp atsignp params)
  (interpret-bind-defaults () params
    (let ((*print-pretty* (or colonp *print-pretty*))
          (*print-level* (unless atsignp *print-level*))
          (*print-length* (unless atsignp *print-length*)))
      (output-object (next-arg) stream))))

;;;; format interpreters and support functions for integer output

;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives. The parameters are interpreted as defined for ~D.
(defun format-print-integer (stream number print-commas-p print-sign-p
                             radix mincol padchar commachar commainterval)
  (let ((*print-base* radix)
        (*print-radix* nil))
    (if (integerp number)
        (let* ((text (princ-to-string (abs number)))
               (commaed (if print-commas-p
                            (format-add-commas text commachar commainterval)
                            text))
               (signed (cond ((minusp number)
                              (concatenate 'string "-" commaed))
                             (print-sign-p
                              (concatenate 'string "+" commaed))
                             (t commaed))))
          ;; colinc = 1, minpad = 0, padleft = t
          (format-write-field stream signed mincol 1 0 padchar t))
        (princ number stream))))

;;; Interpreter stub
(defun format-integer (object base stream)
  (let ((*print-base* base)
        (*print-radix* nil))
    (princ object stream)))

(defun format-add-commas (string commachar commainterval)
  (let ((length (length string)))
    (multiple-value-bind (commas extra) (truncate (1- length) commainterval)
      (let ((new-string (make-string (+ length commas)))
            (first-comma (1+ extra)))
        (replace new-string string :end1 first-comma :end2 first-comma)
        (do ((src first-comma (+ src commainterval))
             (dst first-comma (+ dst commainterval 1)))
            ((= src length))
          (setf (schar new-string dst) commachar)
          (replace new-string string :start1 (1+ dst)
                   :start2 src :end2 (+ src commainterval)))
        new-string))))

(defmacro interpret-format-integer (base)
  `(if (or colonp atsignp params)
       (interpret-bind-defaults
           ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
           params
         (format-print-integer stream (next-arg) colonp atsignp ,base mincol
                               padchar commachar commainterval))
       (let ((*print-base* ,base)
             (*print-radix* nil))
         (princ (next-arg) stream))))

(def-format-interpreter #\D (colonp atsignp params)
  (interpret-format-integer 10))

(def-format-interpreter #\B (colonp atsignp params)
  (interpret-format-integer 2))

(def-format-interpreter #\O (colonp atsignp params)
  (interpret-format-integer 8))

(def-format-interpreter #\X (colonp atsignp params)
  (interpret-format-integer 16))

(def-format-interpreter #\R (colonp atsignp params)
  (interpret-bind-defaults
      ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
       (commainterval 3))
      params
    (let ((arg (next-arg)))
      (unless (or base (integerp arg))
        (format-error "~S is not of type INTEGER." arg))
      (if base
          (format-print-integer stream arg colonp atsignp base mincol
                                padchar commachar commainterval)
          (if atsignp
              (if colonp
                  (format-print-old-roman stream arg)
                  (format-print-roman stream arg))
              (if colonp
                  (format-print-ordinal stream arg)
                  (format-print-cardinal stream arg)))))))

(defconstant-eqx +cardinal-tens+
  #(nil nil "twenty" "thirty" "forty"
        "fifty" "sixty" "seventy" "eighty" "ninety")
  #'equalp)

(defconstant-eqx +cardinal-teens+
  #("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen")
  #'equalp)

(defun format-print-small-cardinal
    (stream n &aux (.cardinal-ones.
                    #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
  (multiple-value-bind (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref .cardinal-ones. hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem)
        (write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones) (truncate rem 10)
        (cond ((< 1 tens)
              (write-string (svref +cardinal-tens+ tens) stream)
              (when (plusp ones)
                (write-char #\- stream)
                (write-string (svref .cardinal-ones. ones) stream)))
             ((= tens 1)
              (write-string (svref +cardinal-teens+ ones) stream))
             ((plusp ones)
              (write-string (svref .cardinal-ones. ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
         (write-string "negative " stream)
         (format-print-cardinal-aux stream (- n) 0 n))
        ((zerop n)
         (write-string "zero" stream))
        (t
         (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux
    (stream n period err
     &aux (.cardinal-periods.
           #("" " thousand" " million" " billion" " trillion" " quadrillion"
             " quintillion" " sextillion" " septillion" " octillion" " nonillion"
             " decillion" " undecillion" " duodecillion" " tredecillion"
             " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
             " octodecillion" " novemdecillion" " vigintillion")))
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 21)
      (error "Number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)
        (write-char #\space stream))
      (format-print-small-cardinal stream here)
      (write-string (svref .cardinal-periods. period) stream))))

(defun format-print-ordinal
    (stream n &aux (.ordinal-ones.
                    #(nil "first" "second" "third" "fourth"
                      "fifth" "sixth" "seventh" "eighth" "ninth"))
                   (.ordinal-tens.
                    #(nil "tenth" "twentieth" "thirtieth" "fortieth"
                      "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth")))
  (when (minusp n)
    (write-string "negative " stream))
  (let ((number (abs n)))
    (multiple-value-bind (top bot) (truncate number 100)
      (unless (zerop top)
        (format-print-cardinal stream (- number bot)))
      (when (and (plusp top) (plusp bot))
        (write-char #\space stream))
      (multiple-value-bind (tens ones) (truncate bot 10)
        (cond ((= bot 12) (write-string "twelfth" stream))
              ((= tens 1)
               (write-string (svref +cardinal-teens+ ones) stream);;;RAD
               (write-string "th" stream))
              ((and (zerop tens) (plusp ones))
               (write-string (svref .ordinal-ones. ones) stream))
              ((and (zerop ones)(plusp tens))
               (write-string (svref .ordinal-tens. tens) stream))
              ((plusp bot)
               (write-string (svref +cardinal-tens+ tens) stream)
               (write-char #\- stream)
               (write-string (svref .ordinal-ones. ones) stream))
              ((plusp number)
               (write-string "th" stream))
              (t
               (write-string "zeroth" stream)))))))

;;; Print Roman numerals

(defun format-print-old-roman (stream n)
  (unless (< 0 n 5000)
    (error "Number too large to print in old Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn
                                (write-char cur-char stream)
                                (- i cur-val))))
                    ((< i cur-val) i))))
      ((zerop start))))

(defun format-print-roman (stream n)
  (unless (< 0 n 4000)
    (error "Number too large to print in Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn
                                (write-char cur-char stream)
                                (- i cur-val))))
                    ((< i cur-val)
                     (cond ((<= (- cur-val cur-sub-val) i)
                            (write-char cur-sub-char stream)
                            (write-char cur-char stream)
                            (- i (- cur-val cur-sub-val)))
                           (t i))))))
          ((zerop start))))

;;;; plural

(def-format-interpreter #\P (colonp atsignp params)
  (interpret-bind-defaults () params
    (let ((arg (if colonp
                   (if (eq orig-args args)
                       (format-error "No previous argument")
                       (do ((arg-ptr orig-args (cdr arg-ptr)))
                           ((eq (cdr arg-ptr) args)
                            (car arg-ptr))))
                   (next-arg))))
      (if atsignp
          (write-string (if (eql arg 1) "y" "ies") stream)
          (unless (eql arg 1) (write-char #\s stream))))))

;;;; format interpreters and support functions for floating point output

(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

;;; TODO: many of the the CHECK-MODIFIER calls in the directive interpreters
;;; can be checked at tokenization time, which benefits from the fact that
;;; a string is only tokenized once (unless evicted from cache).
;;; Repeated calls using the same format control need not repeatedly check
;;; for correctness of the string.
(def-format-interpreter #\F (colonp atsignp params)
  (check-modifier "colon" colonp)
  (interpret-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
                           params
    (format-fixed stream (next-arg) w d k ovf pad atsignp)))

(defun format-fixed (stream number w d k ovf pad atsign)
  (typecase number
    (float
     (format-fixed-aux stream number w d k ovf pad atsign))
    (rational
     (format-fixed-aux stream (coerce number 'single-float)
                       w d k ovf pad atsign))
    (number
     (format-write-field stream (decimal-string number) w 1 0 #\space t))
    (t
     (let ((*print-base* 10))
      (format-princ stream number nil nil w 1 0 pad)))))

;;; We return true if we overflowed, so that ~G can output the overflow char
;;; instead of spaces.
(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (declare (type float number))
  (cond
    ((or (float-infinity-p number)
         (float-nan-p number))
     (prin1 number stream)
     nil)
    (t
     (sb-impl::string-dispatch (single-float double-float)
         number
       (let ((spaceleft w))
         (when (and w (or atsign (float-sign-bit-set-p number)))
           (decf spaceleft))
         (multiple-value-bind (str len lpoint tpoint)
             (sb-impl::flonum-to-string (abs number) spaceleft d k)
           ;; if caller specifically requested no fraction digits, suppress the
           ;; optional trailing zero
           (when (and d (zerop d))
             (setq tpoint nil))
           (when w
             (decf spaceleft len)
             ;; optional leading zero
             (when lpoint
               (if (or (> spaceleft 0) tpoint) ;force at least one digit
                   (decf spaceleft)
                   (setq lpoint nil)))
             ;; optional trailing zero
             (when tpoint
               (if (> spaceleft 0)
                   (decf spaceleft)
                   (setq tpoint nil))))
           (cond ((and w (< spaceleft 0) ovf)
                  ;; field width overflow
                  (dotimes (i w)
                    (write-char ovf stream))
                  t)
                 (t
                  (when w
                    (dotimes (i spaceleft)
                      (write-char pad stream)))
                  (if (float-sign-bit-set-p number)
                      (write-char #\- stream)
                      (when atsign
                        (write-char #\+ stream)))
                  (when lpoint
                    (write-char #\0 stream))
                  (write-string str stream)
                  (when tpoint
                    (write-char #\0 stream))
                  nil))))))))

(def-format-interpreter #\E (colonp atsignp params)
  (check-modifier "colon" colonp)
  (interpret-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    (format-exponential stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-exponential (stream number w d e k ovf pad marker atsign)
  (if (numberp number)
      (if (floatp number)
          (format-exp-aux stream number w d e k ovf pad marker atsign)
          (if (rationalp number)
              (format-exp-aux stream
                              (coerce number 'single-float)
                              w d e k ovf pad marker atsign)
              (format-write-field stream
                                  (decimal-string number)
                                  w 1 0 #\space t)))
      (let ((*print-base* 10))
        (format-princ stream number nil nil w 1 0 pad))))

(defun format-exponent-marker (number)
  (if (case *read-default-float-format*
        ((short-float single-float)
         (typep number 'single-float))
        ((double-float #-long-float long-float)
         (typep number 'double-float))
        #+long-float
        (long-float
         (typep number 'long-float)))
      #\e
      (typecase number
        (single-float #\f)
        (double-float #\d)
        (short-float #\s)
        (long-float #\l))))

;;; Here we prevent the scale factor from shifting all significance out of
;;; a number to the right. We allow insignificant zeroes to be shifted in
;;; to the left right, athough it is an error to specify k and d such that this
;;; occurs. Perhaps we should detect both these condtions and flag them as
;;; errors. As for now, we let the user get away with it, and merely guarantee
;;; that at least one significant digit will appear.

;;; Raymond Toy writes: The Hyperspec seems to say that the exponent
;;; marker is always printed. Make it so. Also, the original version
;;; causes errors when printing infinities or NaN's. The Hyperspec is
;;; silent here, so let's just print out infinities and NaN's instead
;;; of causing an error.
(defun format-exp-aux (stream number w d e k ovf pad marker atsign)
  (declare (type float number))
  (if (or (float-infinity-p number)
          (float-nan-p number))
      (prin1 number stream)
      (multiple-value-bind (num expt) (sb-impl::scale-exponent (abs number))
        (let* ((k (if (= num $1.0) (1- k) k))
               (expt (- expt k))
               (estr (decimal-string (abs expt)))
               (elen (if e (max (length estr) e) (length estr)))
               spaceleft)
          (when w
            (setf spaceleft (- w 2 elen))
            (when (or atsign (float-sign-bit-set-p number))
              (decf spaceleft)))
          (if (and w ovf e (> elen e))  ;exponent overflow
              (dotimes (i w) (write-char ovf stream))
              (let* ((fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
                     (fmin (if (minusp k) 1 fdig)))
                (multiple-value-bind (fstr flen lpoint tpoint)
                    (sb-impl::flonum-to-string num spaceleft fdig k fmin)
                  (when (eql fdig 0) (setq tpoint nil))
                  (when w
                    (decf spaceleft flen)
                    (when lpoint
                      (if (or (> spaceleft 0) tpoint)
                          (decf spaceleft)
                          (setq lpoint nil)))
                    (when tpoint
                      (if (<= spaceleft 0)
                          (setq tpoint nil)
                          (decf spaceleft))))
                  (cond ((and w (< spaceleft 0) ovf)
                         ;;significand overflow
                         (dotimes (i w) (write-char ovf stream)))
                        (t (when w
                             (dotimes (i spaceleft) (write-char pad stream)))
                           (if (float-sign-bit-set-p number)
                               (write-char #\- stream)
                               (if atsign (write-char #\+ stream)))
                           (when lpoint (write-char #\0 stream))
                           (write-string fstr stream)
                           (when tpoint (write-char #\0 stream))
                           (write-char (if marker
                                           marker
                                           (format-exponent-marker number))
                                       stream)
                           (write-char (if (minusp expt) #\- #\+) stream)
                           (when e
                             ;;zero-fill before exponent if necessary
                             (dotimes (i (- e (length estr)))
                               (write-char #\0 stream)))
                           (write-string estr stream))))))))))

(def-format-interpreter #\G (colonp atsignp params)
  (check-modifier "colon" colonp)
  (interpret-bind-defaults
      ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
      params
    (format-general stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-general (stream number w d e k ovf pad marker atsign)
  (if (numberp number)
      (if (floatp number)
          (format-general-aux stream number w d e k ovf pad marker atsign)
          (if (rationalp number)
              (format-general-aux stream
                                  (coerce number 'single-float)
                                  w d e k ovf pad marker atsign)
              (format-write-field stream
                                  (decimal-string number)
                                  w 1 0 #\space t)))
      (let ((*print-base* 10))
        (format-princ stream number nil nil w 1 0 pad))))

;;; Raymond Toy writes: same change as for format-exp-aux
(defun format-general-aux (stream number w d e k ovf pad marker atsign)
  (declare (type float number))
  (if (or (float-infinity-p number)
          (float-nan-p number))
      (prin1 number stream)
      (multiple-value-bind (ignore n) (sb-impl::scale-exponent (abs number))
        (declare (ignore ignore))
        ;; KLUDGE: Default d if omitted. The procedure is taken directly from
        ;; the definition given in the manual, and is not very efficient, since
        ;; we generate the digits twice. Future maintainers are encouraged to
        ;; improve on this. -- rtoy?? 1998??
        (unless d
          (multiple-value-bind (str len)
              (sb-impl::flonum-to-string (abs number))
            (declare (ignore str))
            (let ((q (if (= len 1) 1 (1- len))))
              (setq d (max q (min n 7))))))
        (let* ((ee (if e (+ e 2) 4))
               (ww (if w (- w ee) nil))
               (dd (- d n)))
          (cond ((<= 0 dd d)
                 (let ((char (if (format-fixed-aux stream number ww dd nil
                                                   ovf pad atsign)
                                 ovf
                                 #\space)))
                   (dotimes (i ee) (write-char char stream))))
                (t
                 (format-exp-aux stream number w d e (or k 1)
                                 ovf pad marker atsign)))))))

(def-format-interpreter #\$ (colonp atsignp params)
  (interpret-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    (format-dollars stream (next-arg) d n w pad colonp atsignp)))

(defun format-dollars (stream number d n w pad colon atsign)
  (when (rationalp number)
    ;; This coercion to SINGLE-FLOAT seems as though it gratuitously
    ;; loses precision (why not LONG-FLOAT?) but it's the default
    ;; behavior in the ANSI spec, so in some sense it's the right
    ;; thing, and at least the user shouldn't be surprised.
    (setq number (coerce number 'single-float)))
  (if (floatp number)
      (let* ((signstr (if (float-sign-bit-set-p number)
                          "-"
                          (if atsign "+" "")))
             (signlen (length signstr)))
        (multiple-value-bind (str strlen ig2 ig3 pointplace)
            (sb-impl::flonum-to-string (abs number) nil d nil)
          (declare (ignore ig2 ig3 strlen))
          (when colon
            (write-string signstr stream))
          (dotimes (i (- w signlen (max n pointplace) 1 d))
            (write-char pad stream))
          (unless colon
            (write-string signstr stream))
          (dotimes (i (- n pointplace))
            (write-char #\0 stream))
          (write-string str stream)))
      (let ((*print-base* 10))
        (format-write-field stream
                            (princ-to-string number)
                            w 1 0 #\space t))))

;;;; FORMAT interpreters and support functions for line/page breaks etc.

(def-format-interpreter #\% (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (terpri stream))))

(def-format-interpreter #\& (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (interpret-bind-defaults ((count 1)) params
    (when (plusp count)
      (fresh-line stream)
      (dotimes (i (1- count))
       (terpri stream)))))

(def-format-interpreter #\| (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char (code-char form-feed-char-code) stream))))

(def-format-interpreter #\~ (colonp atsignp params)
  (check-modifier "colon" colonp)
  (check-modifier "at-sign" atsignp)
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char #\~ stream))))

;;; We'll only get here when the directive usage is illegal.
;;; COMBINE-DIRECTIVES would have handled a legal directive.
(def-complex-format-interpreter #\newline (colonp atsignp params directives)
  (check-modifier '("colon" "at-sign") (and colonp atsignp))
  (interpret-bind-defaults () params)
  (bug "Unreachable ~S" directives))

;;;; format interpreters and support functions for tabs and simple pretty
;;;; printing

(def-format-interpreter #\T (colonp atsignp params)
  (if colonp
      (interpret-bind-defaults ((n 1) (m 1)) params
        (pprint-tab (if atsignp :section-relative :section) n m stream))
      (if atsignp
          (interpret-bind-defaults ((colrel 1) (colinc 1)) params
            (format-relative-tab stream colrel colinc))
          (interpret-bind-defaults ((colnum 1) (colinc 1)) params
            (format-absolute-tab stream colnum colinc)))))

(defun output-spaces (stream n)
  (let ((spaces #.(make-string 100 :initial-element #\space)))
    (loop
      (when (< n (length spaces))
        (return))
      (write-string spaces stream)
      (decf n (length spaces)))
    (write-string spaces stream :end n)))

(defun format-relative-tab (stream colrel colinc)
  (if (sb-pretty:pretty-stream-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (let* ((cur (sb-impl::charpos stream))
             (spaces (if (and cur (plusp colinc))
                         (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
                         colrel)))
        (output-spaces stream spaces))))

(defun format-absolute-tab (stream colnum colinc)
  (if (sb-pretty:pretty-stream-p stream)
      (pprint-tab :line colnum colinc stream)
      (let ((cur (sb-impl::charpos stream)))
        (cond ((null cur)
               (write-string "  " stream))
              ((< cur colnum)
               (output-spaces stream (- colnum cur)))
              (t
               (unless (zerop colinc)
                 (output-spaces stream
                                (- colinc (rem (- cur colnum) colinc)))))))))

(def-format-interpreter #\_ (colonp atsignp params)
  (interpret-bind-defaults () params
    (pprint-newline (if colonp
                        (if atsignp
                            :mandatory
                            :fill)
                        (if atsignp
                            :miser
                            :linear))
                    stream)))

(def-format-interpreter #\I (colonp atsignp params)
  (check-modifier "at-sign" atsignp)
  (interpret-bind-defaults ((n 0)) params
    (pprint-indent (if colonp :current :block) n stream)))

;;;; format interpreter for ~*

(def-format-interpreter #\* (colonp atsignp params)
  (check-modifier '("colon" "at-sign") (and colonp atsignp))
  (flet ((lose (index)
           (format-error "Index ~W is out of bounds. It should have ~
                          been between 0 and ~W."
                         index (length orig-args))))
    (if atsignp
        (interpret-bind-defaults ((posn 0)) params
          (if (<= 0 posn (length orig-args))
              (setf args (nthcdr posn orig-args))
              (lose posn)))
        (if colonp
            (interpret-bind-defaults ((n 1)) params
              (do ((cur-posn 0 (1+ cur-posn))
                   (arg-ptr orig-args (cdr arg-ptr)))
                  ((eq arg-ptr args)
                   (let ((new-posn (- cur-posn n)))
                     (if (<= 0 new-posn (length orig-args))
                         (setf args (nthcdr new-posn orig-args))
                         (lose new-posn))))))
            (interpret-bind-defaults ((n 1)) params
              (dotimes (i n)
                (next-arg)))))))

;;;; format interpreter for indirection

(def-format-interpreter #\? (colonp atsignp params string end)
  (check-modifier "colon" colonp)
  (interpret-bind-defaults () params
    (handler-bind
        ((format-error
          (lambda (condition)
            (format-error-at*
             string (1- end)
             "~A~%while processing indirect format string:" (list condition)
             :print-banner nil))))
      (if atsignp
          (setf args (%format stream (next-arg) orig-args args))
          (%format stream (next-arg) (next-arg))))))

;;;; format interpreters for capitalization

(def-complex-format-interpreter #\( (colonp atsignp params directives)
  (let ((close (or (find-directive directives #\) nil)
                   (format-error "No corresponding close paren"))))
    (interpret-bind-defaults () params
      (let* ((posn (position close directives))
             (before (subseq directives 0 posn))
             (after (nthcdr (1+ posn) directives))
             (stream (make-case-frob-stream stream
                                            (if colonp
                                                (if atsignp
                                                    :upcase
                                                    :capitalize)
                                                (if atsignp
                                                    :capitalize-first
                                                    :downcase)))))
        (setf args (interpret-directive-list stream before orig-args args))
        after))))

(def-complex-format-interpreter #\) ()
  (format-error "no corresponding open paren"))

;;;; format interpreters and support functions for conditionalization

(def-complex-format-interpreter #\[ (colonp atsignp params directives)
  (check-modifier '("colon" "at-sign") (and atsignp colonp))
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (setf args
          (cond
            (atsignp
             (when (cdr sublists)
               (format-error "Can only specify one section"))
             (interpret-bind-defaults () params
               (let ((prev-args args)
                     (arg (next-arg)))
                 (if arg
                     (interpret-directive-list
                      stream (car sublists) orig-args prev-args)
                     args))))
            (colonp
             (unless (= (length sublists) 2)
               (format-error "Must specify exactly two sections"))
             (interpret-bind-defaults () params
               (if (next-arg)
                   (interpret-directive-list stream (car sublists)
                                             orig-args args)
                   (interpret-directive-list stream (cadr sublists)
                                             orig-args args))))
            (t
             (interpret-bind-defaults ((index (next-arg))) params
               (let ((default (and last-semi-with-colon-p
                                   (pop sublists)))
                     (last (1- (length sublists))))
                 (unless (integerp index)
                   (format-error
                    "The argument to ~~[ is not an integer: ~A" index))
                 (interpret-directive-list stream
                                           (if (<= 0 index last)
                                               (nth (- last index) sublists)
                                               default)
                                           orig-args
                                           args))))))
    remaining))

(def-complex-format-interpreter #\; ()
  (format-error "~~; not contained within either ~~[...~~] or ~~<...~~>"))

(def-complex-format-interpreter #\] ()
  (format-error "No corresponding open bracket"))

;;;; format interpreter for up-and-out

(defvar *outside-args*)

(def-format-interpreter #\^ (colonp atsignp params)
  (check-modifier "at-sign" atsignp)
  (when (and colonp (not *up-up-and-out-allowed*))
    (format-error "Attempt to use ~~:^ outside a ~~:{...~~} construct"))
  (when (interpret-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
          (cond (arg3 (<= arg1 arg2 arg3))
                (arg2 (eql arg1 arg2))
                (arg1 (eql arg1 0))
                (t (if colonp
                       (null *outside-args*)
                       (null args)))))
    (throw (if colonp 'up-up-and-out 'up-and-out)
           args)))

;;;; format interpreters for iteration

(def-complex-format-interpreter #\{
                                (colonp atsignp params string end directives)
  (let ((close (or (find-directive directives #\} nil)
                   (format-error "No corresponding close brace"))))
    (interpret-bind-defaults ((max-count nil)) params
      (let* ((closed-with-colon (directive-colonp close))
             (posn (position close directives))
             (insides (if (zerop posn)
                          (next-arg)
                          (subseq directives 0 posn)))
             (*up-up-and-out-allowed* colonp))
        (labels
            ((do-guts (orig-args args)
               (if (zerop posn)
                   (handler-bind
                       ((format-error
                         (lambda (condition)
                           (format-error-at*
                            string (1- end)
                            "~A~%while processing indirect format string:"
                            (list condition)
                            :print-banner nil))))
                     (%format stream insides orig-args args))
                   (interpret-directive-list stream insides
                                             orig-args args)))
             (bind-args (orig-args args)
               (if colonp
                   (let* ((arg (next-arg))
                          (*logical-block-popper* nil)
                          (*outside-args* args))
                     (catch 'up-and-out
                       (do-guts arg arg))
                     args)
                   (do-guts orig-args args)))
             (do-loop (orig-args args)
               (catch (if colonp 'up-up-and-out 'up-and-out)
                 (loop
                   (when (and (not closed-with-colon) (null args))
                     (return))
                   (when (and max-count (minusp (decf max-count)))
                     (return))
                   (setf args (bind-args orig-args args))
                   (when (and closed-with-colon (null args))
                     (return)))
                 args)))
          (if atsignp
              (setf args (do-loop orig-args args))
              (let ((arg (next-arg))
                    (*logical-block-popper* nil))
                (do-loop arg arg)))
          (nthcdr (1+ posn) directives))))))

(def-complex-format-interpreter #\} ()
  (format-error "No corresponding open brace"))

;;;; format interpreters and support functions for justification

(def-complex-format-interpreter #\<
                                (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
      (parse-format-justification directives)
    (setf args
          (if (directive-colonp close) ; logical block vs. justification
              (multiple-value-bind (prefix per-line-p insides suffix)
                  (parse-format-logical-block segments colonp first-semi
                                              close params string end)
                (interpret-format-logical-block stream orig-args args
                                                prefix per-line-p insides
                                                suffix atsignp))
              (let ((count (reduce #'+ (mapcar (lambda (x) (count-if #'illegal-inside-justification-p x)) segments))))
                (when (> count 0)
                  ;; ANSI specifies that "an error is signalled" in this
                  ;; situation.
                  (format-error*
                   "~D illegal directive~:P found inside justification block"
                   (list count)
                   :references '((:ansi-cl :section (22 3 5 2)))))
                ;; ANSI does not explicitly say that an error should
                ;; be signalled, but the @ modifier is not explicitly
                ;; allowed for ~> either.
                (when (directive-atsignp close)
                  (format-error-at*
                   nil (1- (directive-end close))
                   "@ modifier not allowed in close directive of ~
                    justification block (i.e. ~~<...~~@>."
                   '()
                   :references '((:ansi-cl :section (22 3 6 2)))))
                (interpret-format-justification stream orig-args args
                                                segments colonp atsignp
                                                first-semi params))))
    remaining))

(defun interpret-format-justification
       (stream orig-args args segments colonp atsignp first-semi params)
  (interpret-bind-defaults
      ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
      params
    (let ((newline-string nil)
          (strings nil)
          (extra-space 0)
          (line-len 0))
      (setf args
            (catch 'up-and-out
              (when (and first-semi (directive-colonp first-semi))
                (interpret-bind-defaults
                    ((extra 0)
                     (len (or (sb-impl::line-length stream) 72)))
                    (directive-params first-semi)
                  (setf newline-string
                        (%with-output-to-string (stream)
                          (setf args
                                (interpret-directive-list stream
                                                          (pop segments)
                                                          orig-args
                                                          args))))
                  (setf extra-space extra)
                  (setf line-len len)))
              (dolist (segment segments)
                (push (%with-output-to-string (stream)
                        (setf args
                              (interpret-directive-list stream segment
                                                        orig-args args)))
                      strings))
              args))
      (format-justification stream newline-string extra-space line-len strings
                            colonp atsignp mincol colinc minpad padchar)))
  args)

(defun format-justification (stream newline-prefix extra-space line-len strings
                             pad-left pad-right mincol colinc minpad padchar)
  (setf strings (reverse strings))
  (let* ((num-gaps (+ (1- (length strings))
                      (if pad-left 1 0)
                      (if pad-right 1 0)))
         (chars (+ (* num-gaps minpad)
                   (loop
                     for string in strings
                     summing (length string))))
         (length (if (> chars mincol)
                     (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
                     mincol))
         (padding (+ (- length chars) (* num-gaps minpad))))
    (when (and newline-prefix
               (> (+ (or (sb-impl::charpos stream) 0)
                     length extra-space)
                  line-len))
      (write-string newline-prefix stream))
    (flet ((do-padding ()
             (let ((pad-len
                    (if (zerop num-gaps) padding (truncate padding num-gaps))))
               (decf padding pad-len)
               (decf num-gaps)
               (dotimes (i pad-len) (write-char padchar stream)))))
      (when (or pad-left (and (not pad-right) (null (cdr strings))))
        (do-padding))
      (when strings
        (write-string (car strings) stream)
        (dolist (string (cdr strings))
          (do-padding)
          (write-string string stream)))
      (when pad-right
        (do-padding)))))

(defun interpret-format-logical-block
       (stream orig-args args prefix per-line-p insides suffix atsignp)
  (let ((arg (if atsignp args (next-arg))))
    (if per-line-p
        (pprint-logical-block
            (stream arg :per-line-prefix prefix :suffix suffix)
          (let ((*logical-block-popper* (lambda () (pprint-pop))))
            (catch 'up-and-out
              (interpret-directive-list stream insides
                                        (if atsignp orig-args arg)
                                        arg))))
        (pprint-logical-block (stream arg :prefix prefix :suffix suffix)
          (let ((*logical-block-popper* (lambda () (pprint-pop))))
            (catch 'up-and-out
              (interpret-directive-list stream insides
                                        (if atsignp orig-args arg)
                                        arg))))))
  (if atsignp nil args))

(defun princ-multiple-to-string (&rest args)
  (%with-output-to-string (str)
    (let ((*print-escape* nil)
          (*print-readably* nil))
      (do-rest-arg ((arg) args)
        (typecase arg
          (string
           (write-string arg str))
          (character
           (write-char arg str))
          (t
           (output-object arg str)))))))

;;;; format interpreter and support functions for user-defined method

(def-format-interpreter #\/ (string start end colonp atsignp params)
  (let ((symbol (or (directive-function .directive)
                    (the symbol (extract-user-fun-name string start end)))))
    (collect ((args))
      (dolist (param-and-offset params)
        (let ((param (cdr param-and-offset)))
          (case param
            (:arg (args (next-arg)))
            (:remaining (args (length args)))
            (t (args param)))))
      (apply symbol stream (next-arg) colonp atsignp (args)))))

(!defun-from-collected-cold-init-forms !format-directives-init)

(defvar sb-int::**tokenize-control-string-cache-vector**-stats) ; might not be DEFVARed
(defun sb-impl::!format-cold-init ()
  (!late-format-init)
  (!format-directives-init)
  ;; cold-init requires these assignments if hash-cache profiling is enabled
  (setq **tokenize-control-string-cache-vector** (make-array 128 :initial-element 0))
  (setq sb-int::**tokenize-control-string-cache-vector**-stats
        (make-array 3 :initial-element 0 :element-type 'fixnum)))

(push '("SB-FORMAT"
        def-format-directive def-complex-format-directive
        def-format-interpreter def-complex-format-interpreter
        interpret-bind-defaults interpret-format-integer next-arg)
      *!removable-symbols*)
