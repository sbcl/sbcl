;;;; the printer

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; exported printer control variables

;;; FIXME: Many of these have nontrivial types, e.g. *PRINT-LEVEL*,
;;; *PRINT-LENGTH*, and *PRINT-LINES* are (OR NULL UNSIGNED-BYTE).

(defvar *print-readably* nil
  #!+sb-doc
  "If true, all objects will printed readably. If readable printing is
  impossible, an error will be signalled. This overrides the value of
  *PRINT-ESCAPE*.")
(defvar *print-escape* T
  #!+sb-doc
  "Should we print in a reasonably machine-readable way? (possibly
  overridden by *PRINT-READABLY*)")
(defvar *print-pretty* nil ; (set later when pretty-printer is initialized)
  #!+sb-doc
  "Should pretty printing be used?")
(defvar *print-base* 10.
  #!+sb-doc
  "the output base for RATIONALs (including integers)")
(defvar *print-radix* nil
  #!+sb-doc
  "Should base be verified when printing RATIONALs?")
(defvar *print-level* nil
  #!+sb-doc
  "How many levels should be printed before abbreviating with \"#\"?")
(defvar *print-length* nil
  #!+sb-doc
  "How many elements at any level should be printed before abbreviating
  with \"...\"?")
(defvar *print-circle* nil
  #!+sb-doc
  "Should we use #n= and #n# notation to preserve uniqueness in general (and
  circularity in particular) when printing?")
(defvar *print-case* :upcase
  #!+sb-doc
  "What case should the printer should use default?")
(defvar *print-array* t
  #!+sb-doc
  "Should the contents of arrays be printed?")
(defvar *print-gensym* t
  #!+sb-doc
  "Should #: prefixes be used when printing symbols with null SYMBOL-PACKAGE?")
(defvar *print-lines* nil
  #!+sb-doc
  "the maximum number of lines to print per object")
(defvar *print-right-margin* nil
  #!+sb-doc
  "the position of the right margin in ems (for pretty-printing)")
(defvar *print-miser-width* nil
  #!+sb-doc
  "If the remaining space between the current column and the right margin
   is less than this, then print using ``miser-style'' output. Miser
   style conditional newlines are turned on, and all indentations are
   turned off. If NIL, never use miser mode.")
(defvar *print-pprint-dispatch* nil
  #!+sb-doc
  "the pprint-dispatch-table that controls how to pretty-print objects")

(defmacro with-standard-io-syntax (&body body)
  #!+sb-doc
  "Bind the reader and printer control variables to values that enable READ
   to reliably read the results of PRINT. These values are:
       *PACKAGE*			the COMMON-LISP-USER package
       *PRINT-ARRAY*			T
       *PRINT-BASE*			10
       *PRINT-CASE*			:UPCASE
       *PRINT-CIRCLE*			NIL
       *PRINT-ESCAPE*			T
       *PRINT-GENSYM*			T
       *PRINT-LENGTH*			NIL
       *PRINT-LEVEL*			NIL
       *PRINT-LINES*			NIL
       *PRINT-MISER-WIDTH*		NIL
       *PRINT-PRETTY*			NIL
       *PRINT-RADIX*			NIL
       *PRINT-READABLY*			T
       *PRINT-RIGHT-MARGIN*		NIL
       *READ-BASE*			10
       *READ-DEFAULT-FLOAT-FORMAT* 	SINGLE-FLOAT
       *READ-EVAL*			T
       *READ-SUPPRESS*			NIL
       *READTABLE*			the standard readtable"
  `(%with-standard-io-syntax (lambda () ,@body)))

(defun %with-standard-io-syntax (function)
  (let ((*package* (find-package "COMMON-LISP-USER"))
	(*print-array* t)
	(*print-base* 10)
	(*print-case* :upcase)
	(*print-circle* nil)
	(*print-escape* t)
	(*print-gensym* t)
	(*print-length* nil)
	(*print-level* nil)
	(*print-lines* nil)
	(*print-miser-width* nil)
	(*print-pretty* nil)
	(*print-radix* nil)
	(*print-readably* t)
	(*print-right-margin* nil)
	(*read-base* 10)
	(*read-default-float-format* 'single-float)
	(*read-eval* t)
	(*read-suppress* nil)
	;; FIXME: It doesn't seem like a good idea to expose our
	;; disaster-recovery *STANDARD-READTABLE* here. What if some
	;; enterprising user corrupts the disaster-recovery readtable
	;; by doing destructive readtable operations within
	;; WITH-STANDARD-IO-SYNTAX? Perhaps we should do a
	;; COPY-READTABLE? The consing would be unfortunate, though.
	(*readtable* *standard-readtable*))
    (funcall function)))

;;;; routines to print objects

(defun write (object &key
		     ((:stream stream) *standard-output*)
		     ((:escape *print-escape*) *print-escape*)
		     ((:radix *print-radix*) *print-radix*)
		     ((:base *print-base*) *print-base*)
		     ((:circle *print-circle*) *print-circle*)
		     ((:pretty *print-pretty*) *print-pretty*)
		     ((:level *print-level*) *print-level*)
		     ((:length *print-length*) *print-length*)
		     ((:case *print-case*) *print-case*)
		     ((:array *print-array*) *print-array*)
		     ((:gensym *print-gensym*) *print-gensym*)
		     ((:readably *print-readably*) *print-readably*)
		     ((:right-margin *print-right-margin*)
		      *print-right-margin*)
		     ((:miser-width *print-miser-width*)
		      *print-miser-width*)
		     ((:lines *print-lines*) *print-lines*)
		     ((:pprint-dispatch *print-pprint-dispatch*)
		      *print-pprint-dispatch*))
  #!+sb-doc
  "Output OBJECT to the specified stream, defaulting to *STANDARD-OUTPUT*"
  (output-object object (out-synonym-of stream))
  object)

(defun prin1 (object &optional stream)
  #!+sb-doc
  "Output a mostly READable printed representation of OBJECT on the specified
  STREAM."
  (let ((*print-escape* T))
    (output-object object (out-synonym-of stream)))
  object)

(defun princ (object &optional stream)
  #!+sb-doc
  "Output an aesthetic but not necessarily READable printed representation
  of OBJECT on the specified STREAM."
  (let ((*print-escape* NIL)
	(*print-readably* NIL))
    (output-object object (out-synonym-of stream)))
  object)

(defun print (object &optional stream)
  #!+sb-doc
  "Output a newline, the mostly READable printed representation of OBJECT, and
  space to the specified STREAM."
  (let ((stream (out-synonym-of stream)))
    (terpri stream)
    (prin1 object stream)
    (write-char #\space stream)
    object))

(defun pprint (object &optional stream)
  #!+sb-doc
  "Prettily output OBJECT preceded by a newline."
  (let ((*print-pretty* t)
	(*print-escape* t)
	(stream (out-synonym-of stream)))
    (terpri stream)
    (output-object object stream))
  (values))

(defun write-to-string
       (object &key
	       ((:escape *print-escape*) *print-escape*)
	       ((:radix *print-radix*) *print-radix*)
	       ((:base *print-base*) *print-base*)
	       ((:circle *print-circle*) *print-circle*)
	       ((:pretty *print-pretty*) *print-pretty*)
	       ((:level *print-level*) *print-level*)
	       ((:length *print-length*) *print-length*)
	       ((:case *print-case*) *print-case*)
	       ((:array *print-array*) *print-array*)
	       ((:gensym *print-gensym*) *print-gensym*)
	       ((:readably *print-readably*) *print-readably*)
	       ((:right-margin *print-right-margin*) *print-right-margin*)
	       ((:miser-width *print-miser-width*) *print-miser-width*)
	       ((:lines *print-lines*) *print-lines*)
	       ((:pprint-dispatch *print-pprint-dispatch*)
		*print-pprint-dispatch*))
  #!+sb-doc
  "Return the printed representation of OBJECT as a string."
  (stringify-object object))

(defun prin1-to-string (object)
  #!+sb-doc
  "Return the printed representation of OBJECT as a string with
   slashification on."
  (stringify-object object t))

(defun princ-to-string (object)
  #!+sb-doc
  "Return the printed representation of OBJECT as a string with
  slashification off."
  (stringify-object object nil))

;;; This produces the printed representation of an object as a string.
;;; The few ...-TO-STRING functions above call this.
(defvar *string-output-streams* ())
(defun stringify-object (object &optional (*print-escape* *print-escape*))
  (let ((stream (if *string-output-streams*
		    (pop *string-output-streams*)
		    (make-string-output-stream))))
    (setup-printer-state)
    (output-object object stream)
    (prog1
	(get-output-stream-string stream)
      (push stream *string-output-streams*))))

;;;; support for the PRINT-UNREADABLE-OBJECT macro

;;; guts of PRINT-UNREADABLE-OBJECT
(defun %print-unreadable-object (object stream type identity body)
  (when *print-readably*
    (error 'print-not-readable :object object))
  (flet ((print-description ()
	   (when type
	     (write (type-of object) :stream stream :circle nil
		    :level nil :length nil)
	     (when (or body identity)
	       (write-char #\space stream)
	       (pprint-newline :fill stream)))
	   (when body
	     (funcall body))
	   (when identity
	     (when body
	       (write-char #\space stream)
	       (pprint-newline :fill stream))
	     (write-char #\{ stream)
	     (write (get-lisp-obj-address object) :stream stream
		    :radix nil :base 16)
	     (write-char #\} stream))))
    (cond ((print-pretty-on-stream-p stream)
	   ;; Since we're printing prettily on STREAM, format the
	   ;; object within a logical block. PPRINT-LOGICAL-BLOCK does
	   ;; not rebind the stream when it is already a pretty stream,
	   ;; so output from the body will go to the same stream.
	   (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
	     (print-description)))
	  (t
            (write-string "#<" stream)
            (print-description)
            (write-char #\> stream))))
  nil)

;;;; circularity detection stuff

;;; When *PRINT-CIRCLE* is T, this gets bound to a hash table that
;;; (eventually) ends up with entries for every object printed. When
;;; we are initially looking for circularities, we enter a T when we
;;; find an object for the first time, and a 0 when we encounter an
;;; object a second time around. When we are actually printing, the 0
;;; entries get changed to the actual marker value when they are first
;;; printed.
(defvar *circularity-hash-table* nil)

;;; When NIL, we are just looking for circularities. After we have
;;; found them all, this gets bound to 0. Then whenever we need a new
;;; marker, it is incremented.
(defvar *circularity-counter* nil)

;;; Check to see whether OBJECT is a circular reference, and return
;;; something non-NIL if it is. If ASSIGN is T, then the number to use
;;; in the #n= and #n# noise is assigned at this time.
;;; If ASSIGN is true, reference bookkeeping will only be done for
;;; existing entries, no new references will be recorded!
;;;
;;; Note: CHECK-FOR-CIRCULARITY must be called *exactly* once with
;;; ASSIGN true, or the circularity detection noise will get confused
;;; about when to use #n= and when to use #n#. If this returns non-NIL
;;; when ASSIGN is true, then you must call HANDLE-CIRCULARITY on it.
;;; If CHECK-FOR-CIRCULARITY returns :INITIATE as the second value,
;;; you need to initiate the circularity detection noise, e.g. bind
;;; *CIRCULARITY-HASH-TABLE* and *CIRCULARITY-COUNTER* to suitable values
;;; (see #'OUTPUT-OBJECT for an example).
(defun check-for-circularity (object &optional assign)
  (cond ((null *print-circle*)
	 ;; Don't bother, nobody cares.
	 nil)
	((null *circularity-hash-table*)
          (values nil :initiate))
	((null *circularity-counter*)
	 (ecase (gethash object *circularity-hash-table*)
	   ((nil)
	    ;; first encounter
	    (setf (gethash object *circularity-hash-table*) t)
	    ;; We need to keep looking.
	    nil)
	   ((t)
	    ;; second encounter
	    (setf (gethash object *circularity-hash-table*) 0)
	    ;; It's a circular reference.
	    t)
	   (0
	    ;; It's a circular reference.
	    t)))
	(t
	 (let ((value (gethash object *circularity-hash-table*)))
	   (case value
	     ((nil t)
	      ;; If NIL, we found an object that wasn't there the
	      ;; first time around. If T, this object appears exactly
	      ;; once. Either way, just print the thing without any
	      ;; special processing. Note: you might argue that
	      ;; finding a new object means that something is broken,
	      ;; but this can happen. If someone uses the ~@<...~:>
	      ;; format directive, it conses a new list each time
	      ;; though format (i.e. the &REST list), so we will have
	      ;; different cdrs.
	      nil)
	     (0
	      (if assign
		  (let ((value (incf *circularity-counter*)))
		    ;; first occurrence of this object: Set the counter.
		    (setf (gethash object *circularity-hash-table*) value)
		    value)
		  t))
	     (t
	      ;; second or later occurrence
	      (- value)))))))

;;; Handle the results of CHECK-FOR-CIRCULARITY. If this returns T then
;;; you should go ahead and print the object. If it returns NIL, then
;;; you should blow it off.
(defun handle-circularity (marker stream)
  (case marker
    (:initiate
     ;; Someone forgot to initiate circularity detection.
     (let ((*print-circle* nil))
       (error "trying to use CHECK-FOR-CIRCULARITY when ~
	       circularity checking isn't initiated")))
    ((t)
     ;; It's a second (or later) reference to the object while we are
     ;; just looking. So don't bother groveling it again.
     nil)
    (t
     (write-char #\# stream)
     (let ((*print-base* 10) (*print-radix* nil))
       (cond ((minusp marker)
	      (output-integer (- marker) stream)
	      (write-char #\# stream)
	      nil)
	     (t
	      (output-integer marker stream)
	      (write-char #\= stream)
	      t))))))

;;;; OUTPUT-OBJECT -- the main entry point

;;; the current pretty printer. This should be either a function that
;;; takes two arguments (the object and the stream) or NIL to indicate
;;; that there is no pretty printer installed.
(defvar *pretty-printer* nil)

;;; Objects whose print representation identifies them EQLly don't
;;; need to be checked for circularity.
(defun uniquely-identified-by-print-p (x)
  (or (numberp x)
      (characterp x)
      (and (symbolp x)
	   (symbol-package x))))

;;; Output OBJECT to STREAM observing all printer control variables.
(defun output-object (object stream)
  (labels ((print-it (stream)
	     (if *print-pretty*
		 (if *pretty-printer*
		     (funcall *pretty-printer* object stream)
		     (let ((*print-pretty* nil))
		       (output-ugly-object object stream)))
		 (output-ugly-object object stream)))
	   (check-it (stream)
             (multiple-value-bind (marker initiate)
                 (check-for-circularity object t)
               ;; initialization of the circulation detect noise ...
	       (if (eq initiate :initiate)
                 (let ((*circularity-hash-table*
			 (make-hash-table :test 'eq)))
                   (check-it (make-broadcast-stream))
                   (let ((*circularity-counter* 0))
                     (check-it stream)))
                 ;; otherwise
                 (if marker
                   (when (handle-circularity marker stream)
                     (print-it stream))
                   (print-it stream))))))
    (cond (;; Maybe we don't need to bother with circularity detection.
	   (or (not *print-circle*)
	       (uniquely-identified-by-print-p object))
	   (print-it stream))
	  (;; If we have already started circularity detection, this
	   ;; object might be a shared reference. If we have not, then
	   ;; if it is a compound object it might contain a circular
	   ;; reference to itself or multiple shared references.
	   (or *circularity-hash-table*
	       (compound-object-p object))
	   (check-it stream))
	  (t
	   (print-it stream)))))

;;; a hack to work around recurring gotchas with printing while
;;; DEFGENERIC PRINT-OBJECT is being built
;;;
;;; (hopefully will go away naturally when CLOS moves into cold init)
(defvar *print-object-is-disabled-p*)

;;; Output OBJECT to STREAM observing all printer control variables
;;; except for *PRINT-PRETTY*. Note: if *PRINT-PRETTY* is non-NIL,
;;; then the pretty printer will be used for any components of OBJECT,
;;; just not for OBJECT itself.
(defun output-ugly-object (object stream)
  (typecase object
    ;; KLUDGE: The TYPECASE approach here is non-ANSI; the ANSI definition of
    ;; PRINT-OBJECT says it provides printing and we're supposed to provide
    ;; PRINT-OBJECT methods covering all classes. We deviate from this
    ;; by using PRINT-OBJECT only when we print instance values. However,
    ;; ANSI makes it hard to tell that we're deviating from this:
    ;;   (1) ANSI specifies that the user isn't supposed to call PRINT-OBJECT
    ;;       directly.
    ;;   (2) ANSI (section 11.1.2.1.2) says it's undefined to define
    ;;       a method on an external symbol in the CL package which is
    ;;       applicable to arg lists containing only direct instances of
    ;;       standardized classes.
    ;; Thus, in order for the user to detect our sleaziness in conforming
    ;; code, he has to do something relatively obscure like
    ;;   (1) actually use tools like FIND-METHOD to look for PRINT-OBJECT
    ;;       methods, or
    ;;   (2) define a PRINT-OBJECT method which is specialized on the stream
    ;;       value (e.g. a Gray stream object).
    ;; As long as no one comes up with a non-obscure way of detecting this
    ;; sleaziness, fixing this nonconformity will probably have a low
    ;; priority. -- WHN 2001-11-25
    (fixnum
     (output-integer object stream))
    (list
     (if (null object)
	 (output-symbol object stream)
	 (output-list object stream)))
    (instance
     (cond ((not (and (boundp '*print-object-is-disabled-p*)
		      *print-object-is-disabled-p*))
	    (print-object object stream))
	   ((typep object 'structure-object)
	    (default-structure-print object stream *current-level*))
	   (t
	    (write-string "#<INSTANCE but not STRUCTURE-OBJECT>" stream))))
    (function
     (unless (and (funcallable-instance-p object)
		  (printed-as-funcallable-standard-class object stream))
       (output-fun object stream)))
    (symbol
     (output-symbol object stream))
    (number
     (etypecase object
       (integer
	(output-integer object stream))
       (float
	(output-float object stream))
       (ratio
	(output-ratio object stream))
       (ratio
	(output-ratio object stream))
       (complex
	(output-complex object stream))))
    (character
     (output-character object stream))
    (vector
     (output-vector object stream))
    (array
     (output-array object stream))
    (system-area-pointer
     (output-sap object stream))
    (weak-pointer
     (output-weak-pointer object stream))
    (lra
     (output-lra object stream))
    (code-component
     (output-code-component object stream))
    (fdefn
     (output-fdefn object stream))
    (t
     (output-random object stream))))

;;;; symbols

;;; values of *PRINT-CASE* and (READTABLE-CASE *READTABLE*) the last
;;; time the printer was called
(defvar *previous-case* nil)
(defvar *previous-readtable-case* nil)

;;; This variable contains the current definition of one of three
;;; symbol printers. SETUP-PRINTER-STATE sets this variable.
(defvar *internal-symbol-output-fun* nil)

;;; This function sets the internal global symbol
;;; *INTERNAL-SYMBOL-OUTPUT-FUN* to the right function depending on
;;; the value of *PRINT-CASE*. See the manual for details. The print
;;; buffer stream is also reset.
(defun setup-printer-state ()
  (unless (and (eq *print-case* *previous-case*)
	       (eq (readtable-case *readtable*) *previous-readtable-case*))
    (setq *previous-case* *print-case*)
    (setq *previous-readtable-case* (readtable-case *readtable*))
    (unless (member *print-case* '(:upcase :downcase :capitalize))
      (setq *print-case* :upcase)
      (error "invalid *PRINT-CASE* value: ~S" *previous-case*))
    (unless (member *previous-readtable-case*
		    '(:upcase :downcase :invert :preserve))
      (setf (readtable-case *readtable*) :upcase)
      (error "invalid READTABLE-CASE value: ~S" *previous-readtable-case*))

    (setq *internal-symbol-output-fun*
	  (case *previous-readtable-case*
	    (:upcase
	     (case *print-case*
	       (:upcase #'output-preserve-symbol)
	       (:downcase #'output-lowercase-symbol)
	       (:capitalize #'output-capitalize-symbol)))
	    (:downcase
	     (case *print-case*
	       (:upcase #'output-uppercase-symbol)
	       (:downcase #'output-preserve-symbol)
	       (:capitalize #'output-capitalize-symbol)))
	    (:preserve #'output-preserve-symbol)
	    (:invert #'output-invert-symbol)))))

;;; Output PNAME (a symbol-name or package-name) surrounded with |'s,
;;; and with any embedded |'s or \'s escaped.
(defun output-quoted-symbol-name (pname stream)
  (write-char #\| stream)
  (dotimes (index (length pname))
    (let ((char (schar pname index)))
      (when (or (char= char #\\) (char= char #\|))
	(write-char #\\ stream))
      (write-char char stream)))
  (write-char #\| stream))

(defun output-symbol (object stream)
  (if (or *print-escape* *print-readably*)
      (let ((package (symbol-package object))
	    (name (symbol-name object)))
	(cond
	 ;; The ANSI spec "22.1.3.3.1 Package Prefixes for Symbols"
	 ;; requires that keywords be printed with preceding colons
	 ;; always, regardless of the value of *PACKAGE*.
	 ((eq package *keyword-package*)
	  (write-char #\: stream))
	 ;; Otherwise, if the symbol's home package is the current
	 ;; one, then a prefix is never necessary.
	 ((eq package (sane-package)))
	 ;; Uninterned symbols print with a leading #:.
	 ((null package)
	  (when (or *print-gensym* *print-readably*)
	    (write-string "#:" stream)))
	 (t
	  (multiple-value-bind (symbol accessible)
	      (find-symbol name (sane-package))
	    ;; If we can find the symbol by looking it up, it need not
	    ;; be qualified. This can happen if the symbol has been
	    ;; inherited from a package other than its home package.
	    (unless (and accessible (eq symbol object))
	      (output-symbol-name (package-name package) stream)
	      (multiple-value-bind (symbol externalp)
		  (find-external-symbol name package)
		(declare (ignore symbol))
		(if externalp
		    (write-char #\: stream)
		    (write-string "::" stream)))))))
	(output-symbol-name name stream))
      (output-symbol-name (symbol-name object) stream nil)))

;;; Output the string NAME as if it were a symbol name. In other
;;; words, diddle its case according to *PRINT-CASE* and
;;; READTABLE-CASE.
(defun output-symbol-name (name stream &optional (maybe-quote t))
  (declare (type simple-base-string name))
  (setup-printer-state)
  (if (and maybe-quote (symbol-quotep name))
      (output-quoted-symbol-name name stream)
      (funcall *internal-symbol-output-fun* name stream)))

;;;; escaping symbols

;;; When we print symbols we have to figure out if they need to be
;;; printed with escape characters. This isn't a whole lot easier than
;;; reading symbols in the first place.
;;;
;;; For each character, the value of the corresponding element is a
;;; fixnum with bits set corresponding to attributes that the
;;; character has. At characters have at least one bit set, so we can
;;; search for any character with a positive test.
(defvar *character-attributes*
  (make-array char-code-limit
	      :element-type '(unsigned-byte 16)
	      :initial-element 0))
(declaim (type (simple-array (unsigned-byte 16) (#.char-code-limit))
	       *character-attributes*))

;;; constants which are a bit-mask for each interesting character attribute
(defconstant other-attribute		(ash 1 0)) ; Anything else legal.
(defconstant number-attribute		(ash 1 1)) ; A numeric digit.
(defconstant uppercase-attribute 	(ash 1 2)) ; An uppercase letter.
(defconstant lowercase-attribute 	(ash 1 3)) ; A lowercase letter.
(defconstant sign-attribute		(ash 1 4)) ; +-
(defconstant extension-attribute 	(ash 1 5)) ; ^_
(defconstant dot-attribute 		(ash 1 6)) ; .
(defconstant slash-attribute		(ash 1 7)) ; /
(defconstant funny-attribute		(ash 1 8)) ; Anything illegal.

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; LETTER-ATTRIBUTE is a local of SYMBOL-QUOTEP. It matches letters
;;; that don't need to be escaped (according to READTABLE-CASE.)
(defparameter *attribute-names*
  `((number . number-attribute) (lowercase . lowercase-attribute)
    (uppercase . uppercase-attribute) (letter . letter-attribute)
    (sign . sign-attribute) (extension . extension-attribute)
    (dot . dot-attribute) (slash . slash-attribute)
    (other . other-attribute) (funny . funny-attribute)))

) ; EVAL-WHEN

(flet ((set-bit (char bit)
	 (let ((code (char-code char)))
	   (setf (aref *character-attributes* code)
		 (logior bit (aref *character-attributes* code))))))

  (dolist (char '(#\! #\@ #\$ #\% #\& #\* #\= #\~ #\[ #\] #\{ #\}
		  #\? #\< #\>))
    (set-bit char other-attribute))

  (dotimes (i 10)
    (set-bit (digit-char i) number-attribute))

  (do ((code (char-code #\A) (1+ code))
       (end (char-code #\Z)))
      ((> code end))
    (declare (fixnum code end))
    (set-bit (code-char code) uppercase-attribute)
    (set-bit (char-downcase (code-char code)) lowercase-attribute))

  (set-bit #\- sign-attribute)
  (set-bit #\+ sign-attribute)
  (set-bit #\^ extension-attribute)
  (set-bit #\_ extension-attribute)
  (set-bit #\. dot-attribute)
  (set-bit #\/ slash-attribute)

  ;; Mark anything not explicitly allowed as funny.
  (dotimes (i char-code-limit)
    (when (zerop (aref *character-attributes* i))
      (setf (aref *character-attributes* i) funny-attribute))))

;;; For each character, the value of the corresponding element is the
;;; lowest base in which that character is a digit.
(defvar *digit-bases*
  (make-array char-code-limit
	      :element-type '(unsigned-byte 8)
	      :initial-element 36))
(declaim (type (simple-array (unsigned-byte 8) (#.char-code-limit))
	       *digit-bases*))

(dotimes (i 36)
  (let ((char (digit-char i 36)))
    (setf (aref *digit-bases* (char-code char)) i)))

;;; A FSM-like thingie that determines whether a symbol is a potential
;;; number or has evil characters in it.
(defun symbol-quotep (name)
  (declare (simple-string name))
  (macrolet ((advance (tag &optional (at-end t))
	       `(progn
		 (when (= index len)
		   ,(if at-end '(go TEST-SIGN) '(return nil)))
		 (setq current (schar name index)
		       code (char-code current)
		       bits (aref attributes code))
		 (incf index)
		 (go ,tag)))
	     (test (&rest attributes)
		`(not (zerop
		       (the fixnum
			    (logand
			     (logior ,@(mapcar
					(lambda (x)
					  (or (cdr (assoc x
							  *attribute-names*))
					      (error "Blast!")))
					attributes))
			     bits)))))
	     (digitp ()
	       `(< (the fixnum (aref bases code)) base)))

    (prog ((len (length name))
	   (attributes *character-attributes*)
	   (bases *digit-bases*)
	   (base *print-base*)
	   (letter-attribute
	    (case (readtable-case *readtable*)
	      (:upcase uppercase-attribute)
	      (:downcase lowercase-attribute)
	      (t (logior lowercase-attribute uppercase-attribute))))
	   (index 0)
	   (bits 0)
	   (code 0)
	   current)
      (declare (fixnum len base index bits code))
      (advance START t)

     TEST-SIGN ; At end, see whether it is a sign...
      (return (not (test sign)))

     OTHER ; not potential number, see whether funny chars...
      (let ((mask (logxor (logior lowercase-attribute uppercase-attribute
				  funny-attribute)
			  letter-attribute)))
	(do ((i (1- index) (1+ i)))
	    ((= i len) (return-from symbol-quotep nil))
	  (unless (zerop (logand (aref attributes (char-code (schar name i)))
				 mask))
	    (return-from symbol-quotep t))))

     START
      (when (digitp)
	(if (test letter)
	    (advance LAST-DIGIT-ALPHA)
	    (advance DIGIT)))
      (when (test letter number other slash) (advance OTHER nil))
      (when (char= current #\.) (advance DOT-FOUND))
      (when (test sign extension) (advance START-STUFF nil))
      (return t)

     DOT-FOUND ; leading dots...
      (when (test letter) (advance START-DOT-MARKER nil))
      (when (digitp) (advance DOT-DIGIT))
      (when (test number other) (advance OTHER nil))
      (when (test extension slash sign) (advance START-DOT-STUFF nil))
      (when (char= current #\.) (advance DOT-FOUND))
      (return t)

     START-STUFF ; leading stuff before any dot or digit
      (when (digitp)
	(if (test letter)
	    (advance LAST-DIGIT-ALPHA)
	    (advance DIGIT)))
      (when (test number other) (advance OTHER nil))
      (when (test letter) (advance START-MARKER nil))
      (when (char= current #\.) (advance START-DOT-STUFF nil))
      (when (test sign extension slash) (advance START-STUFF nil))
      (return t)

     START-MARKER ; number marker in leading stuff...
      (when (test letter) (advance OTHER nil))
      (go START-STUFF)

     START-DOT-STUFF ; leading stuff containing dot without digit...
      (when (test letter) (advance START-DOT-STUFF nil))
      (when (digitp) (advance DOT-DIGIT))
      (when (test sign extension dot slash) (advance START-DOT-STUFF nil))
      (when (test number other) (advance OTHER nil))
      (return t)

     START-DOT-MARKER ; number marker in leading stuff with dot..
      ;; leading stuff containing dot without digit followed by letter...
      (when (test letter) (advance OTHER nil))
      (go START-DOT-STUFF)

     DOT-DIGIT ; in a thing with dots...
      (when (test letter) (advance DOT-MARKER))
      (when (digitp) (advance DOT-DIGIT))
      (when (test number other) (advance OTHER nil))
      (when (test sign extension dot slash) (advance DOT-DIGIT))
      (return t)

     DOT-MARKER ; number marker in number with dot...
      (when (test letter) (advance OTHER nil))
      (go DOT-DIGIT)

     LAST-DIGIT-ALPHA ; previous char is a letter digit...
      (when (or (digitp) (test sign slash))
	(advance ALPHA-DIGIT))
      (when (test letter number other dot) (advance OTHER nil))
      (return t)

     ALPHA-DIGIT ; seen a digit which is a letter...
      (when (or (digitp) (test sign slash))
	(if (test letter)
	    (advance LAST-DIGIT-ALPHA)
	    (advance ALPHA-DIGIT)))
      (when (test letter) (advance ALPHA-MARKER))
      (when (test number other dot) (advance OTHER nil))
      (return t)

     ALPHA-MARKER ; number marker in number with alpha digit...
      (when (test letter) (advance OTHER nil))
      (go ALPHA-DIGIT)

     DIGIT ; seen only ordinary (non-alphabetic) numeric digits...
      (when (digitp)
	(if (test letter)
	    (advance ALPHA-DIGIT)
	    (advance DIGIT)))
      (when (test number other) (advance OTHER nil))
      (when (test letter) (advance MARKER))
      (when (test extension slash sign) (advance DIGIT))
      (when (char= current #\.) (advance DOT-DIGIT))
      (return t)

     MARKER ; number marker in a numeric number...
      (when (test letter) (advance OTHER nil))
      (go DIGIT))))

;;;; *INTERNAL-SYMBOL-OUTPUT-FUN*
;;;;
;;;; case hackery: These functions are stored in
;;;; *INTERNAL-SYMBOL-OUTPUT-FUN* according to the values of
;;;; *PRINT-CASE* and READTABLE-CASE.

;;; called when:
;;; READTABLE-CASE	*PRINT-CASE*
;;; :UPCASE		:UPCASE
;;; :DOWNCASE		:DOWNCASE
;;; :PRESERVE		any
(defun output-preserve-symbol (pname stream)
  (declare (simple-string pname))
  (write-string pname stream))

;;; called when:
;;; READTABLE-CASE	*PRINT-CASE*
;;; :UPCASE		:DOWNCASE
(defun output-lowercase-symbol (pname stream)
  (declare (simple-string pname))
  (dotimes (index (length pname))
    (let ((char (schar pname index)))
      (write-char (char-downcase char) stream))))

;;; called when:
;;; READTABLE-CASE	*PRINT-CASE*
;;; :DOWNCASE		:UPCASE
(defun output-uppercase-symbol (pname stream)
  (declare (simple-string pname))
  (dotimes (index (length pname))
    (let ((char (schar pname index)))
      (write-char (char-upcase char) stream))))

;;; called when:
;;; READTABLE-CASE	*PRINT-CASE*
;;; :UPCASE		:CAPITALIZE
;;; :DOWNCASE		:CAPITALIZE
(defun output-capitalize-symbol (pname stream)
  (declare (simple-string pname))
  (let ((prev-not-alpha t)
	(up (eq (readtable-case *readtable*) :upcase)))
    (dotimes (i (length pname))
      (let ((char (char pname i)))
	(write-char (if up
			(if (or prev-not-alpha (lower-case-p char))
			    char
			    (char-downcase char))
			(if prev-not-alpha
			    (char-upcase char)
			    char))
		    stream)
	(setq prev-not-alpha (not (alpha-char-p char)))))))

;;; called when:
;;; READTABLE-CASE	*PRINT-CASE*
;;; :INVERT		any
(defun output-invert-symbol (pname stream)
  (declare (simple-string pname))
  (let ((all-upper t)
	(all-lower t))
    (dotimes (i (length pname))
      (let ((ch (schar pname i)))
	(when (both-case-p ch)
	  (if (upper-case-p ch)
	      (setq all-lower nil)
	      (setq all-upper nil)))))
    (cond (all-upper (output-lowercase-symbol pname stream))
	  (all-lower (output-uppercase-symbol pname stream))
	  (t
	   (write-string pname stream)))))

#|
(defun test1 ()
  (let ((*readtable* (copy-readtable nil)))
    (format t "READTABLE-CASE  Input   Symbol-name~@
	       ----------------------------------~%")
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (setf (readtable-case *readtable*) readtable-case)
      (dolist (input '("ZEBRA" "Zebra" "zebra"))
	(format t "~&:~A~16T~A~24T~A"
		(string-upcase readtable-case)
		input
		(symbol-name (read-from-string input)))))))

(defun test2 ()
  (let ((*readtable* (copy-readtable nil)))
    (format t "READTABLE-CASE  *PRINT-CASE*  Symbol-name  Output  Princ~@
	       --------------------------------------------------------~%")
    (dolist (readtable-case '(:upcase :downcase :preserve :invert))
      (setf (readtable-case *readtable*) readtable-case)
      (dolist (*print-case* '(:upcase :downcase :capitalize))
	(dolist (symbol '(|ZEBRA| |Zebra| |zebra|))
	  (format t "~&:~A~15T:~A~29T~A~42T~A~50T~A"
		  (string-upcase readtable-case)
		  (string-upcase *print-case*)
		  (symbol-name symbol)
		  (prin1-to-string symbol)
		  (princ-to-string symbol)))))))
|#

;;;; recursive objects

(defun output-list (list stream)
  (descend-into (stream)
    (write-char #\( stream)
    (let ((length 0)
	  (list list))
      (loop
	(punt-print-if-too-long length stream)
	(output-object (pop list) stream)
	(unless list
	  (return))
	(when (or (atom list)
                  (check-for-circularity list))
	  (write-string " . " stream)
	  (output-object list stream)
	  (return))
	(write-char #\space stream)
	(incf length)))
    (write-char #\) stream)))

(defun output-vector (vector stream)
  (declare (vector vector))
  (cond ((stringp vector)
	 (cond ((or *print-escape* *print-readably*)
		(write-char #\" stream)
		(quote-string vector stream)
		(write-char #\" stream))
	       (t
		(write-string vector stream))))
	((not (or *print-array* *print-readably*))
	 (output-terse-array vector stream))
	((bit-vector-p vector)
	 (write-string "#*" stream)
	 (dovector (bit vector)
	   ;; (Don't use OUTPUT-OBJECT here, since this code
	   ;; has to work for all possible *PRINT-BASE* values.)
	   (write-char (if (zerop bit) #\0 #\1) stream)))
	(t
	 (when (and *print-readably*
		    (not (eq (array-element-type vector) t)))
	   (error 'print-not-readable :object vector))
	 (descend-into (stream)
		       (write-string "#(" stream)
		       (dotimes (i (length vector))
			 (unless (zerop i)
			   (write-char #\space stream))
			 (punt-print-if-too-long i stream)
			 (output-object (aref vector i) stream))
		       (write-string ")" stream)))))

;;; This function outputs a string quoting characters sufficiently
;;; so that someone can read it in again. Basically, put a slash in
;;; front of an character satisfying NEEDS-SLASH-P.
(defun quote-string (string stream)
  (macrolet ((needs-slash-p (char)
	       ;; KLUDGE: We probably should look at the readtable, but just do
	       ;; this for now. [noted by anonymous long ago] -- WHN 19991130
	       `(or (char= ,char #\\)
                 (char= ,char #\"))))
    (with-array-data ((data string) (start) (end (length string)))
      (do ((index start (1+ index)))
	  ((>= index end))
	(let ((char (schar data index)))
	  (when (needs-slash-p char) (write-char #\\ stream))
	  (write-char char stream))))))

;;; Output the printed representation of any array in either the #< or #A
;;; form.
(defun output-array (array stream)
  (if (or *print-array* *print-readably*)
      (output-array-guts array stream)
      (output-terse-array array stream)))

;;; Output the abbreviated #< form of an array.
(defun output-terse-array (array stream)
  (let ((*print-level* nil)
	(*print-length* nil))
    (print-unreadable-object (array stream :type t :identity t))))

;;; Output the readable #A form of an array.
(defun output-array-guts (array stream)
  (when (and *print-readably*
	     (not (eq (array-element-type array) t)))
    (error 'print-not-readable :object array))
  (write-char #\# stream)
  (let ((*print-base* 10))
    (output-integer (array-rank array) stream))
  (write-char #\A stream)
  (with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (sub-output-array-guts data (array-dimensions array) stream start)))

(defun sub-output-array-guts (array dimensions stream index)
  (declare (type (simple-array * (*)) array) (fixnum index))
  (cond ((null dimensions)
	 (output-object (aref array index) stream))
	(t
	 (descend-into (stream)
	   (write-char #\( stream)
	   (let* ((dimension (car dimensions))
		  (dimensions (cdr dimensions))
		  (count (reduce #'* dimensions)))
	     (dotimes (i dimension)
	       (unless (zerop i)
		 (write-char #\space stream))
	       (punt-print-if-too-long i stream)
	       (sub-output-array-guts array dimensions stream index)
	       (incf index count)))
	   (write-char #\) stream)))))

;;; a trivial non-generic-function placeholder for PRINT-OBJECT, for
;;; use until CLOS is set up (at which time it will be replaced with
;;; the real generic function implementation)
(defun print-object (instance stream)
  (default-structure-print instance stream *current-level*))

;;;; integer, ratio, and complex printing (i.e. everything but floats)

(defun output-integer (integer stream)
  ;; FIXME: This UNLESS form should be pulled out into something like
  ;; (SANE-PRINT-BASE), along the lines of (SANE-PACKAGE) for the
  ;; *PACKAGE* variable.
  (unless (and (fixnump *print-base*)
	       (< 1 *print-base* 37))
    (let ((obase *print-base*))
      (setq *print-base* 10.)
      (error "~A is not a reasonable value for *PRINT-BASE*." obase)))
  (when (and (not (= *print-base* 10.))
	     *print-radix*)
    ;; First print leading base information, if any.
    (write-char #\# stream)
    (write-char (case *print-base*
		  (2. #\b)
		  (8. #\o)
		  (16. #\x)
		  (T (let ((fixbase *print-base*)
			   (*print-base* 10.)
			   (*print-radix* ()))
		       (sub-output-integer fixbase stream))
		     #\r))
		stream))
  ;; Then output a minus sign if the number is negative, then output
  ;; the absolute value of the number.
  (cond ((bignump integer) (print-bignum integer stream))
	((< integer 0)
	 (write-char #\- stream)
	 (sub-output-integer (- integer) stream))
	(t
	 (sub-output-integer integer stream)))
  ;; Print any trailing base information, if any.
  (if (and (= *print-base* 10.) *print-radix*)
      (write-char #\. stream)))

(defun sub-output-integer (integer stream)
  (let ((quotient ())
	(remainder ()))
    ;; Recurse until you have all the digits pushed on the stack.
    (if (not (zerop (multiple-value-setq (quotient remainder)
		      (truncate integer *print-base*))))
	(sub-output-integer quotient stream))
    ;; Then as each recursive call unwinds, turn the digit (in remainder)
    ;; into a character and output the character.
    (write-char (code-char (if (and (> remainder 9.)
				    (> *print-base* 10.))
			       (+ (char-code #\A) (- remainder 10.))
			       (+ (char-code #\0) remainder)))
		stream)))

;;;; bignum printing

;;; *BASE-POWER* holds the number that we keep dividing into the
;;; bignum for each *print-base*. We want this number as close to
;;; *most-positive-fixnum* as possible, i.e. (floor (log
;;; most-positive-fixnum *print-base*)).
(defparameter *base-power* (make-array 37 :initial-element nil))

;;; *FIXNUM-POWER--1* holds the number of digits for each *PRINT-BASE*
;;; that fit in the corresponding *base-power*.
(defparameter *fixnum-power--1* (make-array 37 :initial-element nil))

;;; Print the bignum to the stream. We first generate the correct
;;; value for *base-power* and *fixnum-power--1* if we have not
;;; already. Then we call bignum-print-aux to do the printing.
(defun print-bignum (big stream)
  (unless (aref *base-power* *print-base*)
    (do ((power-1 -1 (1+ power-1))
	 (new-divisor *print-base* (* new-divisor *print-base*))
	 (divisor 1 new-divisor))
	((not (fixnump new-divisor))
	 (setf (aref *base-power* *print-base*) divisor)
	 (setf (aref *fixnum-power--1* *print-base*) power-1))))
  (bignum-print-aux (cond ((minusp big)
			   (write-char #\- stream)
			   (- big))
			  (t big))
		    (aref *base-power* *print-base*)
		    (aref *fixnum-power--1* *print-base*)
		    stream)
  big)

(defun bignum-print-aux (big divisor power-1 stream)
  (multiple-value-bind (newbig fix) (truncate big divisor)
    (if (fixnump newbig)
	(sub-output-integer newbig stream)
	(bignum-print-aux newbig divisor power-1 stream))
    (do ((zeros power-1 (1- zeros))
	 (base-power *print-base* (* base-power *print-base*)))
	((> base-power fix)
	 (dotimes (i zeros) (write-char #\0 stream))
	 (sub-output-integer fix stream)))))

(defun output-ratio (ratio stream)
  (when *print-radix*
    (write-char #\# stream)
    (case *print-base*
      (2 (write-char #\b stream))
      (8 (write-char #\o stream))
      (16 (write-char #\x stream))
      (t (write *print-base* :stream stream :radix nil :base 10)))
    (write-char #\r stream))
  (let ((*print-radix* nil))
    (output-integer (numerator ratio) stream)
    (write-char #\/ stream)
    (output-integer (denominator ratio) stream)))

(defun output-complex (complex stream)
  (write-string "#C(" stream)
  (output-object (realpart complex) stream)
  (write-char #\space stream)
  (output-object (imagpart complex) stream)
  (write-char #\) stream))

;;;; float printing

;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does
;;; most of the work for all printing of floating point numbers in the
;;; printer and in FORMAT. It converts a floating point number to a
;;; string in a free or fixed format with no exponent. The
;;; interpretation of the arguments is as follows:
;;;
;;;     X	- The floating point number to convert, which must not be
;;;		negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;		of fraction digits to produce if the FDIGITS parameter
;;;		is unspecified or NIL. If the non-fraction digits and the
;;;		decimal point alone exceed this width, no fraction digits
;;;		will be produced unless a non-NIL value of FDIGITS has been
;;;		specified. Field overflow is not considerd an error at this
;;;		level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;		trailing zeroes may be introduced as needed. May be
;;;		unspecified or NIL, in which case as many digits as possible
;;;		are generated, subject to the constraint that there are no
;;;		trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;		printed is (* x (expt 10 scale)). This scaling is exact,
;;;		and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;		number of fraction digits which will be produced, regardless
;;;		of the value of WIDTH or FDIGITS. This feature is used by
;;;		the ~E format directive to prevent complete loss of
;;;		significance in the printed value due to a bogus choice of
;;;		scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;		       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;		       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;		       point. Zero indicates point before first digit.
;;;
;;; NOTE: FLONUM-TO-STRING goes to a lot of trouble to guarantee
;;; accuracy. Specifically, the decimal number printed is the closest
;;; possible approximation to the true value of the binary number to
;;; be printed from among all decimal representations with the same
;;; number of digits. In free-format output, i.e. with the number of
;;; digits unconstrained, it is guaranteed that all the information is
;;; preserved, so that a properly- rounding reader can reconstruct the
;;; original binary number, bit-for-bit, from its printed decimal
;;; representation. Furthermore, only as many digits as necessary to
;;; satisfy this condition will be printed.
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.
;;; The algorithm is essentially that of algorithm Dragon4 in "How to
;;; Print Floating-Point Numbers Accurately" by Steele and White. The
;;; current (draft) version of this paper may be found in
;;; [CMUC]<steele>tradix.press. DO NOT EVEN THINK OF ATTEMPTING TO
;;; UNDERSTAND THIS CODE WITHOUT READING THE PAPER!

(defvar *digits* "0123456789")

(defun flonum-to-string (x &optional width fdigits scale fmin)
  (cond ((zerop x)
	 ;; Zero is a special case which FLOAT-STRING cannot handle.
	 (if fdigits
	     (let ((s (make-string (1+ fdigits) :initial-element #\0)))
	       (setf (schar s 0) #\.)
	       (values s (length s) t (zerop fdigits) 0))
	     (values "." 1 t t 0)))
	(t
	 (multiple-value-bind (sig exp) (integer-decode-float x)
	   (let* ((precision (float-precision x))
		  (digits (float-digits x))
		  (fudge (- digits precision))
		  (width (if width (max width 1) nil)))
	   (float-string (ash sig (- fudge)) (+ exp fudge) precision width
			 fdigits scale fmin))))))

(defun float-string (fraction exponent precision width fdigits scale fmin)
  (let ((r fraction) (s 1) (m- 1) (m+ 1) (k 0)
	(digits 0) (decpnt 0) (cutoff nil) (roundup nil) u low high
	(digit-string (make-array 50
				  :element-type 'base-char
				  :fill-pointer 0
				  :adjustable t)))
    ;; Represent fraction as r/s, error bounds as m+/s and m-/s.
    ;; Rational arithmetic avoids loss of precision in subsequent
    ;; calculations.
    (cond ((> exponent 0)
	   (setq r (ash fraction exponent))
	   (setq m- (ash 1 exponent))
	   (setq m+ m-))
	  ((< exponent 0)
	   (setq s (ash 1 (- exponent)))))
    ;; Adjust the error bounds m+ and m- for unequal gaps.
    (when (= fraction (ash 1 precision))
      (setq m+ (ash m+ 1))
      (setq r (ash r 1))
      (setq s (ash s 1)))
    ;; Scale value by requested amount, and update error bounds.
    (when scale
      (if (minusp scale)
	  (let ((scale-factor (expt 10 (- scale))))
	    (setq s (* s scale-factor)))
	  (let ((scale-factor (expt 10 scale)))
	    (setq r (* r scale-factor))
	    (setq m+ (* m+ scale-factor))
	    (setq m- (* m- scale-factor)))))
    ;; Scale r and s and compute initial k, the base 10 logarithm of r.
    (do ()
	((>= r (ceiling s 10)))
      (decf k)
      (setq r (* r 10))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10)))
    (do ()(nil)
      (do ()
	  ((< (+ (ash r 1) m+) (ash s 1)))
	(setq s (* s 10))
	(incf k))
      ;; Determine number of fraction digits to generate.
      (cond (fdigits
	     ;; Use specified number of fraction digits.
	     (setq cutoff (- fdigits))
	     ;;don't allow less than fmin fraction digits
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin))))
	    (width
	     ;; Use as many fraction digits as width will permit but
	     ;; force at least fmin digits even if width will be
	     ;; exceeded.
	     (if (< k 0)
		 (setq cutoff (- 1 width))
		 (setq cutoff (1+ (- k width))))
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin)))))
      ;; If we decided to cut off digit generation before precision
      ;; has been exhausted, rounding the last digit may cause a carry
      ;; propagation. We can prevent this, preserving left-to-right
      ;; digit generation, with a few magical adjustments to m- and
      ;; m+. Of course, correct rounding is also preserved.
      (when (or fdigits width)
	(let ((a (- cutoff k))
	      (y s))
	  (if (>= a 0)
	      (dotimes (i a) (setq y (* y 10)))
	      (dotimes (i (- a)) (setq y (ceiling y 10))))
	  (setq m- (max y m-))
	  (setq m+ (max y m+))
	  (when (= m+ y) (setq roundup t))))
      (when (< (+ (ash r 1) m+) (ash s 1)) (return)))
    ;; Zero-fill before fraction if no integer part.
    (when (< k 0)
      (setq decpnt digits)
      (vector-push-extend #\. digit-string)
      (dotimes (i (- k))
	(incf digits) (vector-push-extend #\0 digit-string)))
    ;; Generate the significant digits.
    (do ()(nil)
      (decf k)
      (when (= k -1)
	(vector-push-extend #\. digit-string)
	(setq decpnt digits))
      (multiple-value-setq (u r) (truncate (* r 10) s))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10))
      (setq low (< (ash r 1) m-))
      (if roundup
	  (setq high (>= (ash r 1) (- (ash s 1) m+)))
	  (setq high (> (ash r 1) (- (ash s 1) m+))))
      ;; Stop when either precision is exhausted or we have printed as
      ;; many fraction digits as permitted.
      (when (or low high (and cutoff (<= k cutoff))) (return))
      (vector-push-extend (char *digits* u) digit-string)
      (incf digits))
    ;; If cutoff occurred before first digit, then no digits are
    ;; generated at all.
    (when (or (not cutoff) (>= k cutoff))
      ;; Last digit may need rounding
      (vector-push-extend (char *digits*
				(cond ((and low (not high)) u)
				      ((and high (not low)) (1+ u))
				      (t (if (<= (ash r 1) s) u (1+ u)))))
			  digit-string)
      (incf digits))
    ;; Zero-fill after integer part if no fraction.
    (when (>= k 0)
      (dotimes (i k) (incf digits) (vector-push-extend #\0 digit-string))
      (vector-push-extend #\. digit-string)
      (setq decpnt digits))
    ;; Add trailing zeroes to pad fraction if fdigits specified.
    (when fdigits
      (dotimes (i (- fdigits (- digits decpnt)))
	(incf digits)
	(vector-push-extend #\0 digit-string)))
    ;; all done
    (values digit-string (1+ digits) (= decpnt 0) (= decpnt digits) decpnt)))

;;; Given a non-negative floating point number, SCALE-EXPONENT returns
;;; a new floating point number Z in the range (0.1, 1.0] and an
;;; exponent E such that Z * 10^E is (approximately) equal to the
;;; original number. There may be some loss of precision due the
;;; floating point representation. The scaling is always done with
;;; long float arithmetic, which helps printing of lesser precisions
;;; as well as avoiding generic arithmetic.
;;;
;;; When computing our initial scale factor using EXPT, we pull out
;;; part of the computation to avoid over/under flow. When
;;; denormalized, we must pull out a large factor, since there is more
;;; negative exponent range than positive range.
(defun scale-exponent (original-x)
  (let* ((x (coerce original-x 'long-float)))
    (multiple-value-bind (sig exponent) (decode-float x)
      (declare (ignore sig))
      (if (= x 0.0l0)
	  (values (float 0.0l0 original-x) 1)
	  (let* ((ex (round (* exponent (log 2l0 10))))
		 (x (if (minusp ex)
			(if (float-denormalized-p x)
			    #!-long-float
			    (* x 1.0l16 (expt 10.0l0 (- (- ex) 16)))
			    #!+long-float
			    (* x 1.0l18 (expt 10.0l0 (- (- ex) 18)))
			    (* x 10.0l0 (expt 10.0l0 (- (- ex) 1))))
			(/ x 10.0l0 (expt 10.0l0 (1- ex))))))
	    (do ((d 10.0l0 (* d 10.0l0))
		 (y x (/ x d))
		 (ex ex (1+ ex)))
		((< y 1.0l0)
		 (do ((m 10.0l0 (* m 10.0l0))
		      (z y (* y m))
		      (ex ex (1- ex)))
		     ((>= z 0.1l0)
		      (values (float z original-x) ex))))))))))

;;;; entry point for the float printer

;;; the float printer as called by PRINT, PRIN1, PRINC, etc. The
;;; argument is printed free-format, in either exponential or
;;; non-exponential notation, depending on its magnitude.
;;;
;;; NOTE: When a number is to be printed in exponential format, it is
;;; scaled in floating point. Since precision may be lost in this
;;; process, the guaranteed accuracy properties of FLONUM-TO-STRING
;;; are lost. The difficulty is that FLONUM-TO-STRING performs
;;; extensive computations with integers of similar magnitude to that
;;; of the number being printed. For large exponents, the bignums
;;; really get out of hand. If bignum arithmetic becomes reasonably
;;; fast and the exponent range is not too large, then it might become
;;; attractive to handle exponential notation with the same accuracy
;;; as non-exponential notation, using the method described in the
;;; Steele and White paper.

;;; Print the appropriate exponent marker for X and the specified exponent.
(defun print-float-exponent (x exp stream)
  (declare (type float x) (type integer exp) (type stream stream))
  (let ((*print-radix* nil)
	(plusp (plusp exp)))
    (if (typep x *read-default-float-format*)
	(unless (eql exp 0)
	  (format stream "e~:[~;+~]~D" plusp exp))
	(format stream "~C~:[~;+~]~D"
		(etypecase x
		  (single-float #\f)
		  (double-float #\d)
		  (short-float #\s)
		  (long-float #\L))
		plusp exp))))

(defun output-float-infinity (x stream)
  (declare (float x) (stream stream))
  (cond (*read-eval*
         (write-string "#." stream))
        (*print-readably*
         (error 'print-not-readable :object x))
        (t
         (write-string "#<" stream)))
  (write-string "SB-EXT:" stream)
  (write-string (symbol-name (float-format-name x)) stream)
  (write-string (if (plusp x) "-POSITIVE-" "-NEGATIVE-")
                stream)
  (write-string "INFINITY" stream)
  (unless *read-eval*
    (write-string ">" stream)))

(defun output-float-nan (x stream)
  (print-unreadable-object (x stream)
    (princ (float-format-name x) stream)
    (write-string (if (float-trapping-nan-p x) " trapping" " quiet") stream)
    (write-string " NaN" stream)))

;;; the function called by OUTPUT-OBJECT to handle floats
(defun output-float (x stream)
  (cond
   ((float-infinity-p x)
    (output-float-infinity x stream))
   ((float-nan-p x)
    (output-float-nan x stream))
   (t
    (let ((x (cond ((minusp (float-sign x))
		    (write-char #\- stream)
		    (- x))
		   (t
		    x))))
      (cond
       ((zerop x)
	(write-string "0.0" stream)
	(print-float-exponent x 0 stream))
       (t
	(output-float-aux x stream (float 1/1000 x) (float 10000000 x))))))))
(defun output-float-aux (x stream e-min e-max)
  (if (and (>= x e-min) (< x e-max))
      ;; free format
      (multiple-value-bind (str len lpoint tpoint) (flonum-to-string x)
	(declare (ignore len))
	(when lpoint (write-char #\0 stream))
	(write-string str stream)
	(when tpoint (write-char #\0 stream))
	(print-float-exponent x 0 stream))
      ;; exponential format
      (multiple-value-bind (f ex) (scale-exponent x)
	(multiple-value-bind (str len lpoint tpoint)
	    (flonum-to-string f nil nil 1)
	  (declare (ignore len))
	  (when lpoint (write-char #\0 stream))
	  (write-string str stream)
	  (when tpoint (write-char #\0 stream))
	  ;; Subtract out scale factor of 1 passed to FLONUM-TO-STRING.
	  (print-float-exponent x (1- ex) stream)))))

;;;; other leaf objects

;;; If *PRINT-ESCAPE* is false, just do a WRITE-CHAR, otherwise output
;;; the character name or the character in the #\char format.
(defun output-character (char stream)
  (if (or *print-escape* *print-readably*)
      (let ((name (char-name char)))
	(write-string "#\\" stream)
	(if name
	    (quote-string name stream)
	    (write-char char stream)))
      (write-char char stream)))

(defun output-sap (sap stream)
  (declare (type system-area-pointer sap))
  (cond (*read-eval*
	 (format stream "#.(~S #X~8,'0X)" 'int-sap (sap-int sap)))
	(t
	 (print-unreadable-object (sap stream)
	   (format stream "system area pointer: #X~8,'0X" (sap-int sap))))))

(defun output-weak-pointer (weak-pointer stream)
  (declare (type weak-pointer weak-pointer))
  (print-unreadable-object (weak-pointer stream)
    (multiple-value-bind (value validp) (weak-pointer-value weak-pointer)
      (cond (validp
	     (write-string "weak pointer: " stream)
	     (write value :stream stream))
	    (t
	     (write-string "broken weak pointer" stream))))))

(defun output-code-component (component stream)
  (print-unreadable-object (component stream :identity t)
    (let ((dinfo (%code-debug-info component)))
      (cond ((eq dinfo :bogus-lra)
	     (write-string "bogus code object" stream))
	    (t
	     (write-string "code object" stream)
	     (when dinfo
	       (write-char #\space stream)
	       (output-object (sb!c::debug-info-name dinfo) stream)))))))

(defun output-lra (lra stream)
  (print-unreadable-object (lra stream :identity t)
    (write-string "return PC object" stream)))

(defun output-fdefn (fdefn stream)
  (print-unreadable-object (fdefn stream)
    (write-string "FDEFINITION object for " stream)
    (output-object (fdefn-name fdefn) stream)))

;;;; functions

;;; Output OBJECT as using PRINT-OBJECT if it's a
;;; FUNCALLABLE-STANDARD-CLASS, or return NIL otherwise.
;;;
;;; The definition here is a simple temporary placeholder. It will be
;;; overwritten by a smarter version (capable of calling generic
;;; PRINT-OBJECT when appropriate) when CLOS is installed.
(defun printed-as-clos-funcallable-standard-class (object stream)
  (declare (ignore object stream))
  nil)

(defun output-fun (object stream)
  (let* ((*print-length* 3) ; in case we have to..
	 (*print-level* 3)  ; ..print an interpreted function definition
	 ;; FIXME: This find-the-function-name idiom ought to be
	 ;; encapsulated in a function somewhere.
	 (name (case (fun-subtype object)
		 (#.sb!vm:closure-header-widetag "CLOSURE")
		 (#.sb!vm:simple-fun-header-widetag (%simple-fun-name object))
		 (t 'no-name-available)))
	 (identified-by-name-p (and (symbolp name)
				    (fboundp name)
				    (eq (fdefinition name) object))))
      (print-unreadable-object (object
				stream
				:identity (not identified-by-name-p))
	(prin1 'function stream)
	(unless (eq name 'no-name-available)
	  (format stream " ~S" name)))))

;;;; catch-all for unknown things

(defun output-random (object stream)
  (print-unreadable-object (object stream :identity t)
    (let ((lowtag (lowtag-of object)))
      (case lowtag
	(#.sb!vm:other-pointer-lowtag
	  (let ((widetag (widetag-of object)))
	    (case widetag
	      (#.sb!vm:value-cell-header-widetag
	       (write-string "value cell " stream)
	       (output-object (value-cell-ref object) stream))
	      (t
	       (write-string "unknown pointer object, widetag=" stream)
	       (let ((*print-base* 16) (*print-radix* t))
		 (output-integer widetag stream))))))
	((#.sb!vm:fun-pointer-lowtag
	  #.sb!vm:instance-pointer-lowtag
	  #.sb!vm:list-pointer-lowtag)
	 (write-string "unknown pointer object, lowtag=" stream)
	 (let ((*print-base* 16) (*print-radix* t))
	   (output-integer lowtag stream)))
	(t
	 (case (widetag-of object)
	   (#.sb!vm:unbound-marker-widetag
	    (write-string "unbound marker" stream))
	   (t
	    (write-string "unknown immediate object, lowtag=" stream)
	    (let ((*print-base* 2) (*print-radix* t))
	      (output-integer lowtag stream))
	    (write-string ", widetag=" stream)
	    (let ((*print-base* 16) (*print-radix* t))
	      (output-integer (widetag-of object) stream)))))))))
