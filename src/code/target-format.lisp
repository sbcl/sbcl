;;;; functions to implement FORMAT and FORMATTER

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FORMAT")

;;;; FORMAT

(defun format (destination control-string &rest format-arguments)
  #!+sb-doc
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
  (declare (explicit-check))
  (etypecase destination
    (null
     (with-simple-output-to-string (stream)
       (%format stream control-string format-arguments)))
    (string
     (with-simple-output-to-string (stream destination)
       (%format stream control-string format-arguments)))
    ((member t)
     (%format *standard-output* control-string format-arguments)
     nil)
    (stream
     (%format destination control-string format-arguments)
     nil)))

(defun %format (stream string-or-fun orig-args &optional (args orig-args))
  (if (functionp string-or-fun)
      (apply string-or-fun stream args)
      (catch 'up-and-out
        (let* ((string (etypecase string-or-fun
                         (simple-string
                          string-or-fun)
                         (string
                          (coerce string-or-fun 'simple-string))))
               (*default-format-error-control-string* string)
               (*logical-block-popper* nil))
          (interpret-directive-list stream (tokenize-control-string string)
                                    orig-args args)))))

(defun interpret-directive-list (stream directives orig-args args)
  (if directives
      (let ((directive (car directives)))
        (etypecase directive
          (simple-string
           (write-string directive stream)
           (interpret-directive-list stream (cdr directives) orig-args args))
          (format-directive
           (multiple-value-bind (new-directives new-args)
               (let* ((character (format-directive-character directive))
                      (function
                       (typecase character
                         (base-char
                          (svref *format-directive-interpreters* (char-code character)))))
                      (*default-format-error-offset*
                       (1- (format-directive-end directive))))
                 (unless function
                   (error 'format-error
                          :complaint "unknown format directive ~@[(character: ~A)~]"
                          :args (list (char-name character))))
                 (multiple-value-bind (new-directives new-args)
                     (funcall function stream directive
                              (cdr directives) orig-args args)
                   (values new-directives new-args)))
             (interpret-directive-list stream new-directives
                                       orig-args new-args)))))
      args))

;;;; FORMAT directive definition macros and runtime support

(eval-when (:compile-toplevel :execute)

;;; This macro is used to extract the next argument from the current arg list.
;;; This is the version used by format directive interpreters.
(sb!xc:defmacro next-arg (&optional offset)
  `(progn
     (when (null args)
       (error 'format-error
              :complaint "no more arguments"
              ,@(when offset
                  `(:offset ,offset))))
     (when *logical-block-popper*
       (funcall *logical-block-popper*))
     (pop args)))

(sb!xc:defmacro def-complex-format-interpreter (char lambda-list &body body)
  (let ((defun-name
            (intern (format nil
                            "~:@(~:C~)-FORMAT-DIRECTIVE-INTERPRETER"
                            char)))
        (directive (sb!xc:gensym "DIRECTIVE"))
        (directives (if lambda-list (car (last lambda-list)) (sb!xc:gensym "DIRECTIVES"))))
    `(progn
       (defun ,defun-name (stream ,directive ,directives orig-args args)
         (declare (ignorable stream orig-args args))
         ,@(if lambda-list
               `((let ,(mapcar (lambda (var)
                                 `(,var
                                   (,(symbolicate "FORMAT-DIRECTIVE-" var)
                                    ,directive)))
                               (butlast lambda-list))
                   (values (progn ,@body) args)))
               `((declare (ignore ,directive ,directives))
                 ,@body)))
       (%set-format-directive-interpreter ,char #',defun-name))))

(sb!xc:defmacro def-format-interpreter (char lambda-list &body body)
  (let ((directives (sb!xc:gensym "DIRECTIVES")))
    `(def-complex-format-interpreter ,char (,@lambda-list ,directives)
       ,@body
       ,directives)))

(sb!xc:defmacro interpret-bind-defaults (specs params &body body)
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
           (error 'format-error
                  :complaint
                  "too many parameters, expected no more than ~W"
                  :args (list ,(length specs))
                  :offset (caar ,params)))
         ,@body))))

) ; EVAL-WHEN

;;;; format interpreters and support functions for simple output

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (when (and colinc (<= colinc 0))
    (error 'format-error
           :complaint "The value of colinc is ~a, should be a positive integer"
           :args (list colinc)))
  (when (and mincol (< mincol 0))
    (error 'format-error
           :complaint "The value of mincol is ~a, should be a non-negative integer"
           :args (list mincol)))
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
        (error 'format-error
               :complaint "~s is not of type CHARACTER."
               :args (list arg)))
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

(eval-when (:compile-toplevel :execute)
(sb!xc:defmacro interpret-format-integer (base)
  `(if (or colonp atsignp params)
       (interpret-bind-defaults
           ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
           params
         (format-print-integer stream (next-arg) colonp atsignp ,base mincol
                               padchar commachar commainterval))
       (let ((*print-base* ,base)
             (*print-radix* nil))
         (princ (next-arg) stream))))
) ; EVAL-WHEN

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
      (unless (or base
                  (integerp arg))
        (error 'format-error
               :complaint "~s is not of type INTEGER."
               :args (list arg)))
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

(defparameter *cardinal-ones*
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defparameter *cardinal-tens*
  #(nil nil "twenty" "thirty" "forty"
        "fifty" "sixty" "seventy" "eighty" "ninety"))

(defparameter *cardinal-teens*
  #("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defparameter *cardinal-periods*
  #("" " thousand" " million" " billion" " trillion" " quadrillion"
    " quintillion" " sextillion" " septillion" " octillion" " nonillion"
    " decillion" " undecillion" " duodecillion" " tredecillion"
    " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
    " octodecillion" " novemdecillion" " vigintillion"))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth"
        "fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-tens*
  #(nil "tenth" "twentieth" "thirtieth" "fortieth"
        "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

(defun format-print-small-cardinal (stream n)
  (multiple-value-bind (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref *cardinal-ones* hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem)
        (write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones) (truncate rem 10)
        (cond ((< 1 tens)
              (write-string (svref *cardinal-tens* tens) stream)
              (when (plusp ones)
                (write-char #\- stream)
                (write-string (svref *cardinal-ones* ones) stream)))
             ((= tens 1)
              (write-string (svref *cardinal-teens* ones) stream))
             ((plusp ones)
              (write-string (svref *cardinal-ones* ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
         (write-string "negative " stream)
         (format-print-cardinal-aux stream (- n) 0 n))
        ((zerop n)
         (write-string "zero" stream))
        (t
         (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 21)
      (error "number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)
        (write-char #\space stream))
      (format-print-small-cardinal stream here)
      (write-string (svref *cardinal-periods* period) stream))))

(defun format-print-ordinal (stream n)
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
               (write-string (svref *cardinal-teens* ones) stream);;;RAD
               (write-string "th" stream))
              ((and (zerop tens) (plusp ones))
               (write-string (svref *ordinal-ones* ones) stream))
              ((and (zerop ones)(plusp tens))
               (write-string (svref *ordinal-tens* tens) stream))
              ((plusp bot)
               (write-string (svref *cardinal-tens* tens) stream)
               (write-char #\- stream)
               (write-string (svref *ordinal-ones* ones) stream))
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
                       (error 'format-error
                              :complaint "no previous argument")
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

(def-format-interpreter #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
           :complaint
           "cannot specify the colon modifier with this directive"))
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
     (sb!impl::string-dispatch (single-float double-float)
         number
       (let ((spaceleft w))
         (when (and w (or atsign (minusp (float-sign number))))
           (decf spaceleft))
         (multiple-value-bind (str len lpoint tpoint)
             (sb!impl::flonum-to-string (abs number) spaceleft d k)
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
                  (if (minusp (float-sign number))
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
  (when colonp
    (error 'format-error
           :complaint
           "cannot specify the colon modifier with this directive"))
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
        ((double-float #!-long-float long-float)
         (typep number 'double-float))
        #!+long-float
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
      (multiple-value-bind (num expt) (sb!impl::scale-exponent (abs number))
        (let* ((k (if (= num 1.0) (1- k) k))
               (expt (- expt k))
               (estr (decimal-string (abs expt)))
               (elen (if e (max (length estr) e) (length estr)))
               spaceleft)
          (when w
            (setf spaceleft (- w 2 elen))
            (when (or atsign (minusp (float-sign number)))
              (decf spaceleft)))
          (if (and w ovf e (> elen e))  ;exponent overflow
              (dotimes (i w) (write-char ovf stream))
              (let* ((fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
                     (fmin (if (minusp k) 1 fdig)))
                (multiple-value-bind (fstr flen lpoint tpoint)
                    (sb!impl::flonum-to-string num spaceleft fdig k fmin)
                  (when (and d (zerop d)) (setq tpoint nil))
                  (when w
                    (decf spaceleft flen)
                    (when lpoint
                      (if (or (> spaceleft 0) tpoint)
                          (decf spaceleft)
                          (setq lpoint nil)))
                    (when (and tpoint (<= spaceleft 0))
                      (setq tpoint nil)))
                  (cond ((and w (< spaceleft 0) ovf)
                         ;;significand overflow
                         (dotimes (i w) (write-char ovf stream)))
                        (t (when w
                             (dotimes (i spaceleft) (write-char pad stream)))
                           (if (minusp (float-sign number))
                               (write-char #\- stream)
                               (if atsign (write-char #\+ stream)))
                           (when lpoint (write-char #\0 stream))
                           (write-string fstr stream)
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
  (when colonp
    (error 'format-error
           :complaint
           "cannot specify the colon modifier with this directive"))
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
      (multiple-value-bind (ignore n) (sb!impl::scale-exponent (abs number))
        (declare (ignore ignore))
        ;; KLUDGE: Default d if omitted. The procedure is taken directly from
        ;; the definition given in the manual, and is not very efficient, since
        ;; we generate the digits twice. Future maintainers are encouraged to
        ;; improve on this. -- rtoy?? 1998??
        (unless d
          (multiple-value-bind (str len)
              (sb!impl::flonum-to-string (abs number))
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
      (let* ((signstr (if (minusp (float-sign number))
                          "-"
                          (if atsign "+" "")))
             (signlen (length signstr)))
        (multiple-value-bind (str strlen ig2 ig3 pointplace)
            (sb!impl::flonum-to-string number nil d nil)
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
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (terpri stream))))

(def-format-interpreter #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
    (when (plusp count)
      (fresh-line stream)
      (dotimes (i (1- count))
       (terpri stream)))))

(def-format-interpreter #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char (code-char form-feed-char-code) stream))))

(def-format-interpreter #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
           :complaint
           "cannot specify either colon or atsign for this directive"))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char #\~ stream))))

(def-complex-format-interpreter #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
           :complaint
           "cannot specify both colon and atsign for this directive"))
  (interpret-bind-defaults () params
    (when atsignp
      (write-char #\newline stream)))
  (if (and (not colonp)
           directives
           (simple-string-p (car directives)))
      (cons (string-left-trim *format-whitespace-chars*
                              (car directives))
            (cdr directives))
      directives))

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
  (if (sb!pretty:pretty-stream-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (let* ((cur (sb!impl::charpos stream))
             (spaces (if (and cur (plusp colinc))
                         (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
                         colrel)))
        (output-spaces stream spaces))))

(defun format-absolute-tab (stream colnum colinc)
  (if (sb!pretty:pretty-stream-p stream)
      (pprint-tab :line colnum colinc stream)
      (let ((cur (sb!impl::charpos stream)))
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
  (when atsignp
    (error 'format-error
           :complaint "cannot specify the at-sign modifier"))
  (interpret-bind-defaults ((n 0)) params
    (pprint-indent (if colonp :current :block) n stream)))

;;;; format interpreter for ~*

(def-format-interpreter #\* (colonp atsignp params)
  (if atsignp
      (if colonp
          (error 'format-error
                 :complaint "cannot specify both colon and at-sign")
          (interpret-bind-defaults ((posn 0)) params
            (if (<= 0 posn (length orig-args))
                (setf args (nthcdr posn orig-args))
                (error 'format-error
                       :complaint "Index ~W is out of bounds. (It should ~
                                   have been between 0 and ~W.)"
                       :args (list posn (length orig-args))))))
      (if colonp
          (interpret-bind-defaults ((n 1)) params
            (do ((cur-posn 0 (1+ cur-posn))
                 (arg-ptr orig-args (cdr arg-ptr)))
                ((eq arg-ptr args)
                 (let ((new-posn (- cur-posn n)))
                   (if (<= 0 new-posn (length orig-args))
                       (setf args (nthcdr new-posn orig-args))
                       (error 'format-error
                              :complaint
                              "Index ~W is out of bounds. (It should
                               have been between 0 and ~W.)"
                              :args
                              (list new-posn (length orig-args))))))))
          (interpret-bind-defaults ((n 1)) params
            (dotimes (i n)
              (next-arg))))))

;;;; format interpreter for indirection

(def-format-interpreter #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
           :complaint "cannot specify the colon modifier"))
  (interpret-bind-defaults () params
    (handler-bind
        ((format-error
          (lambda (condition)
            (error 'format-error
                   :complaint
                   "~A~%while processing indirect format string:"
                   :args (list condition)
                   :print-banner nil
                   :control-string string
                   :offset (1- end)))))
      (if atsignp
          (setf args (%format stream (next-arg) orig-args args))
          (%format stream (next-arg) (next-arg))))))

;;;; format interpreters for capitalization

(def-complex-format-interpreter #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
             :complaint "no corresponding close paren"))
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
  (error 'format-error
         :complaint "no corresponding open paren"))

;;;; format interpreters and support functions for conditionalization

(def-complex-format-interpreter #\[ (colonp atsignp params directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (setf args
          (if atsignp
              (if colonp
                  (error 'format-error
                         :complaint
                     "cannot specify both the colon and at-sign modifiers")
                  (if (cdr sublists)
                      (error 'format-error
                             :complaint
                             "can only specify one section")
                      (interpret-bind-defaults () params
                        (let ((prev-args args)
                              (arg (next-arg)))
                          (if arg
                              (interpret-directive-list stream
                                                        (car sublists)
                                                        orig-args
                                                        prev-args)
                              args)))))
              (if colonp
                  (if (= (length sublists) 2)
                      (interpret-bind-defaults () params
                        (if (next-arg)
                            (interpret-directive-list stream (car sublists)
                                                      orig-args args)
                            (interpret-directive-list stream (cadr sublists)
                                                      orig-args args)))
                      (error 'format-error
                             :complaint
                             "must specify exactly two sections"))
                  (interpret-bind-defaults ((index (next-arg))) params
                    (let* ((default (and last-semi-with-colon-p
                                         (pop sublists)))
                           (last (1- (length sublists)))
                           (sublist
                            (if (<= 0 index last)
                                (nth (- last index) sublists)
                                default)))
                      (interpret-directive-list stream sublist orig-args
                                                args))))))
    remaining))

(def-complex-format-interpreter #\; ()
  (error 'format-error
         :complaint
         "~~; not contained within either ~~[...~~] or ~~<...~~>"))

(def-complex-format-interpreter #\] ()
  (error 'format-error
         :complaint
         "no corresponding open bracket"))

;;;; format interpreter for up-and-out

(defvar *outside-args*)

(def-format-interpreter #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
           :complaint "cannot specify the at-sign modifier"))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
           :complaint "attempt to use ~~:^ outside a ~~:{...~~} construct"))
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
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
             :complaint
             "no corresponding close brace"))
    (interpret-bind-defaults ((max-count nil)) params
      (let* ((closed-with-colon (format-directive-colonp close))
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
                           (error
                            'format-error
                            :complaint
                            "~A~%while processing indirect format string:"
                            :args (list condition)
                            :print-banner nil
                            :control-string string
                            :offset (1- end)))))
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
  (error 'format-error
         :complaint "no corresponding open brace"))

;;;; format interpreters and support functions for justification

(def-complex-format-interpreter #\<
                                (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
      (parse-format-justification directives)
    (setf args
          (if (format-directive-colonp close) ; logical block vs. justification
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
                  (error 'format-error
                         :complaint "~D illegal directive~:P found inside justification block"
                         :args (list count)
                         :references (list '(:ansi-cl :section (22 3 5 2)))))
                ;; ANSI does not explicitly say that an error should
                ;; be signalled, but the @ modifier is not explicitly
                ;; allowed for ~> either.
                (when (format-directive-atsignp close)
                  (error 'format-error
                         :complaint "@ modifier not allowed in close ~
                         directive of justification ~
                         block (i.e. ~~<...~~@>."
                         :offset (1- (format-directive-end close))
                         :references (list '(:ansi-cl :section (22 3 6 2)))))
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
              (when (and first-semi (format-directive-colonp first-semi))
                (interpret-bind-defaults
                    ((extra 0)
                     (len (or (sb!impl::line-length stream) 72)))
                    (format-directive-params first-semi)
                  (setf newline-string
                        (with-simple-output-to-string (stream)
                          (setf args
                                (interpret-directive-list stream
                                                          (pop segments)
                                                          orig-args
                                                          args))))
                  (setf extra-space extra)
                  (setf line-len len)))
              (dolist (segment segments)
                (push (with-simple-output-to-string (stream)
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
               (> (+ (or (sb!impl::charpos stream) 0)
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

;;;; format interpreter and support functions for user-defined method

(def-format-interpreter #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-fun-name string start end)))
    (collect ((args))
      (dolist (param-and-offset params)
        (let ((param (cdr param-and-offset)))
          (case param
            (:arg (args (next-arg)))
            (:remaining (args (length args)))
            (t (args param)))))
      (apply (fdefinition symbol) stream (next-arg) colonp atsignp (args)))))
