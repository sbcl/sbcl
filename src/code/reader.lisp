;;;; READ and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; miscellaneous global variables

;;; ANSI: "the floating-point format that is to be used when reading a
;;; floating-point number that has no exponent marker or that has e or
;;; E for an exponent marker"
(defvar *read-default-float-format* 'single-float)
(declaim (type (member short-float single-float double-float long-float)
	       *read-default-float-format*))

(defvar *readtable*)
(declaim (type readtable *readtable*))
#!+sb-doc
(setf (fdocumentation '*readtable* 'variable)
       "Variable bound to current readtable.")

;;; a standard Lisp readtable. This is for recovery from broken
;;; read-tables (and for WITH-STANDARD-IO-SYNTAX), and should not
;;; normally be user-visible.
(defvar *standard-readtable*)

(defvar *old-package* nil
  #!+sb-doc
  "the value of *PACKAGE* at the start of the last read, or NIL")

;;; In case we get an error trying to parse a symbol, we want to rebind the
;;; above stuff so it's cool.

;;; FIXME: These forward declarations should be moved somewhere earlier,
;;; or discarded.
(declaim (special *package* *keyword-package* *read-base*))

;;;; reader errors

(defun reader-eof-error (stream context)
  (error 'reader-eof-error
	 :stream stream
	 :context context))

(defun %reader-error (stream control &rest args)
  (error 'reader-error
	 :stream stream
	 :format-control control
	 :format-arguments args))

;;;; macros and functions for character tables

;;; FIXME: could be SB!XC:DEFMACRO inside EVAL-WHEN (COMPILE EVAL)
(defmacro get-cat-entry (char rt)
  ;; KLUDGE: Only give this side-effect-free args.
  ;; FIXME: should probably become inline function
  `(elt (character-attribute-table ,rt)
	(char-code ,char)))

(defun set-cat-entry (char newvalue &optional (rt *readtable*))
  (setf (elt (character-attribute-table rt)
	     (char-code char))
	newvalue))

;;; FIXME: could be SB!XC:DEFMACRO inside EVAL-WHEN (COMPILE EVAL)
(defmacro get-cmt-entry (char rt)
  `(the function
	(elt (the simple-vector (character-macro-table ,rt))
	     (char-code ,char))))

(defun set-cmt-entry (char newvalue &optional (rt *readtable*))
  (setf (elt (the simple-vector (character-macro-table rt))
	     (char-code char))
	(coerce newvalue 'function)))

(defun undefined-macro-char (stream char)
  (unless *read-suppress*
    (%reader-error stream "undefined read-macro character ~S" char)))

;;; The character attribute table is a CHAR-CODE-LIMIT vector of integers.

(defmacro test-attribute (char whichclass rt)
  `(= (the fixnum (get-cat-entry ,char ,rt)) ,whichclass))

;;; predicates for testing character attributes

#!-sb-fluid (declaim (inline whitespacep))
(defun whitespacep (char &optional (rt *readtable*))
  (test-attribute char +char-attr-whitespace+ rt))

(defmacro constituentp (char &optional (rt '*readtable*))
  `(>= (get-cat-entry ,char ,rt) +char-attr-constituent+))

(defmacro terminating-macrop (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-terminating-macro+ ,rt))

(defmacro escapep (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-escape+ ,rt))

(defmacro multiple-escape-p (char &optional (rt '*readtable*))
  `(test-attribute ,char +char-attr-multiple-escape+ ,rt))

(defmacro token-delimiterp (char &optional (rt '*readtable*))
  ;; depends on actual attribute numbering above.
  `(<= (get-cat-entry ,char ,rt) +char-attr-terminating-macro+))

;;;; secondary attribute table

;;; There are a number of "secondary" attributes which are constant
;;; properties of characters (as long as they are constituents).

(defvar *secondary-attribute-table*)
(declaim (type attribute-table *secondary-attribute-table*))

(defun !set-secondary-attribute (char attribute)
  (setf (elt *secondary-attribute-table* (char-code char))
	attribute))

(defun !cold-init-secondary-attribute-table ()
  (setq *secondary-attribute-table*
	(make-array char-code-limit :element-type '(unsigned-byte 8)
		    :initial-element +char-attr-constituent+))
  (!set-secondary-attribute #\: +char-attr-package-delimiter+)
  (!set-secondary-attribute #\| +char-attr-multiple-escape+) ; |) [for EMACS]
  (!set-secondary-attribute #\. +char-attr-constituent-dot+)
  (!set-secondary-attribute #\+ +char-attr-constituent-sign+)
  (!set-secondary-attribute #\- +char-attr-constituent-sign+)
  (!set-secondary-attribute #\/ +char-attr-constituent-slash+)
  (do ((i (char-code #\0) (1+ i)))
      ((> i (char-code #\9)))
    (!set-secondary-attribute (code-char i) +char-attr-constituent-digit+))
  (!set-secondary-attribute #\E +char-attr-constituent-expt+)
  (!set-secondary-attribute #\F +char-attr-constituent-expt+)
  (!set-secondary-attribute #\D +char-attr-constituent-expt+)
  (!set-secondary-attribute #\S +char-attr-constituent-expt+)
  (!set-secondary-attribute #\L +char-attr-constituent-expt+)
  (!set-secondary-attribute #\e +char-attr-constituent-expt+)
  (!set-secondary-attribute #\f +char-attr-constituent-expt+)
  (!set-secondary-attribute #\d +char-attr-constituent-expt+)
  (!set-secondary-attribute #\s +char-attr-constituent-expt+)
  (!set-secondary-attribute #\l +char-attr-constituent-expt+))

(defmacro get-secondary-attribute (char)
  `(elt *secondary-attribute-table*
	(char-code ,char)))

;;;; readtable operations

(defun copy-readtable (&optional (from-readtable *readtable*)
				 (to-readtable (make-readtable)))
  (let ((really-from-readtable (or from-readtable *standard-readtable*)))
    (replace (character-attribute-table to-readtable)
	     (character-attribute-table really-from-readtable))
    (replace (character-macro-table to-readtable)
	     (character-macro-table really-from-readtable))
    (setf (dispatch-tables to-readtable)
	  (mapcar #'(lambda (pair) (cons (car pair)
					 (copy-seq (cdr pair))))
		  (dispatch-tables really-from-readtable)))
    to-readtable))

(defun set-syntax-from-char (to-char from-char &optional
				     (to-readtable *readtable*)
				     (from-readtable ()))
  #!+sb-doc
  "Causes the syntax of TO-CHAR to be the same as FROM-CHAR in the
  optional readtable (defaults to the current readtable). The
  FROM-TABLE defaults to the standard Lisp readtable when NIL."
  (let ((really-from-readtable (or from-readtable *standard-readtable*)))
    ;; Copy FROM-CHAR entries to TO-CHAR entries, but make sure that if
    ;; from char is a constituent you don't copy non-movable secondary
    ;; attributes (constituent types), and that said attributes magically
    ;; appear if you transform a non-constituent to a constituent.
    (let ((att (get-cat-entry from-char really-from-readtable)))
      (if (constituentp from-char really-from-readtable)
	  (setq att (get-secondary-attribute to-char)))
      (set-cat-entry to-char att to-readtable)
      (set-cmt-entry to-char
		     (get-cmt-entry from-char really-from-readtable)
		     to-readtable)))
  t)

(defun set-macro-character (char function &optional
				 (non-terminatingp nil) (rt *readtable*))
  #!+sb-doc
  "Causes char to be a macro character which invokes function when
   seen by the reader. The non-terminatingp flag can be used to
   make the macro character non-terminating. The optional readtable
   argument defaults to the current readtable. Set-macro-character
   returns T."
  (if non-terminatingp
      (set-cat-entry char (get-secondary-attribute char) rt)
      (set-cat-entry char +char-attr-terminating-macro+ rt))
  (set-cmt-entry char function rt)
  T)

(defun get-macro-character (char &optional (rt *readtable*))
  #!+sb-doc
  "Returns the function associated with the specified char which is a macro
  character. The optional readtable argument defaults to the current
  readtable."
  (let ((rt (or rt *standard-readtable*)))
    ;; Check macro syntax, return associated function if it's there.
    ;; Returns a value for all constituents.
    (cond ((constituentp char)
	   (values (get-cmt-entry char rt) t))
	  ((terminating-macrop char)
	   (values (get-cmt-entry char rt) nil))
	  (t nil))))

;;;; definitions to support internal programming conventions

(defmacro eofp (char) `(eq ,char *eof-object*))

(defun flush-whitespace (stream)
  ;; This flushes whitespace chars, returning the last char it read (a
  ;; non-white one). It always gets an error on end-of-file.
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-char stream
	  (do ((attribute-table (character-attribute-table *readtable*))
	       (char (fast-read-char t) (fast-read-char t)))
	      ((/= (the fixnum (aref attribute-table (char-code char)))
		   +char-attr-whitespace+)
	       (done-with-fast-read-char)
	       char)))
	;; fundamental-stream
	(do ((attribute-table (character-attribute-table *readtable*))
	     (char (stream-read-char stream) (stream-read-char stream)))
	    ((or (eq char :eof)
		 (/= (the fixnum (aref attribute-table (char-code char)))
		     +char-attr-whitespace+))
	     (if (eq char :eof)
		 (error 'end-of-file :stream stream)
		 char))))))

;;;; temporary initialization hack

(defun !cold-init-standard-readtable ()
  (setq *standard-readtable* (make-readtable))
  ;; All characters default to "constituent" in MAKE-READTABLE.
  ;; *** un-constituent-ize some of these ***
  (let ((*readtable* *standard-readtable*))
    (set-cat-entry (code-char tab-char-code) +char-attr-whitespace+)
    (set-cat-entry #\linefeed +char-attr-whitespace+)
    (set-cat-entry #\space +char-attr-whitespace+)
    (set-cat-entry (code-char form-feed-char-code) +char-attr-whitespace+)
    (set-cat-entry (code-char return-char-code) +char-attr-whitespace+)
    (set-cat-entry #\\ +char-attr-escape+)
    (set-cmt-entry #\\ #'read-token)
    (set-cat-entry (code-char rubout-char-code) +char-attr-whitespace+)
    (set-cmt-entry #\: #'read-token)
    (set-cmt-entry #\| #'read-token)
    ;; macro definitions
    (set-macro-character #\" #'read-string)
    ;; * # macro
    (set-macro-character #\' #'read-quote)
    (set-macro-character #\( #'read-list)
    (set-macro-character #\) #'read-right-paren)
    (set-macro-character #\; #'read-comment)
    ;; * backquote
    ;; all constituents
    (do ((ichar 0 (1+ ichar))
	 (char))
	((= ichar #O200))
      (setq char (code-char ichar))
      (when (constituentp char *standard-readtable*)
	    (set-cat-entry char (get-secondary-attribute char))
	    (set-cmt-entry char #'read-token)))))

;;;; implementation of the read buffer

(defvar *read-buffer*)
(defvar *read-buffer-length*)
;;; FIXME: Is it really helpful to have *READ-BUFFER-LENGTH* be a
;;; separate variable instead of just calculating it on the fly as
;;; (LENGTH *READ-BUFFER*)?

(defvar *inch-ptr*)
(defvar *ouch-ptr*)

(declaim (type index *read-buffer-length* *inch-ptr* *ouch-ptr*))
(declaim (simple-string *read-buffer*))

(defmacro reset-read-buffer ()
  ;; Turn *READ-BUFFER* into an empty read buffer.
  `(progn
     ;; *OUCH-PTR* always points to next char to write.
     (setq *ouch-ptr* 0)
     ;; *INCH-PTR* always points to next char to read.
     (setq *inch-ptr* 0)))

(defun !cold-init-read-buffer ()
  (setq *read-buffer* (make-string 512)) ; initial bufsize
  (setq *read-buffer-length* 512)
  (reset-read-buffer))

;;; FIXME I removed "THE FIXNUM"'s from OUCH-READ-BUFFER and
;;; OUCH-UNREAD-BUFFER, check to make sure that Python really is smart
;;; enough to make good code without them. And while I'm at it,
;;; converting them from macros to inline functions might be good,
;;; too.

(defmacro ouch-read-buffer (char)
  `(progn
     ;; When buffer overflow
     (when (>= *ouch-ptr* *read-buffer-length*)
       ;; Size should be doubled.
       (grow-read-buffer))
     (setf (elt (the simple-string *read-buffer*) *ouch-ptr*) ,char)
     (setq *ouch-ptr* (1+ *ouch-ptr*))))

;;; macro to move *ouch-ptr* back one.
(defmacro ouch-unread-buffer ()
  '(when (> *ouch-ptr* *inch-ptr*)
     (setq *ouch-ptr* (1- (the fixnum *ouch-ptr*)))))

(defun grow-read-buffer ()
  (let ((rbl (length (the simple-string *read-buffer*))))
    (setq *read-buffer*
	  (concatenate 'simple-string
		       *read-buffer*
		       (make-string rbl)))
    (setq *read-buffer-length* (* 2 rbl))))

(defun inchpeek-read-buffer ()
  (if (>= (the fixnum *inch-ptr*) (the fixnum *ouch-ptr*))
      *eof-object*
      (elt *read-buffer* *inch-ptr*)))

(defun inch-read-buffer ()
  (if (>= *inch-ptr* *ouch-ptr*)
      *eof-object*
      (prog1
	  (elt *read-buffer* *inch-ptr*)
	(incf *inch-ptr*))))

(defmacro unread-buffer ()
  `(decf *inch-ptr*))

(defun read-unwind-read-buffer ()
  ;; Keep contents, but make next (INCH..) return first character.
  (setq *inch-ptr* 0))

(defun read-buffer-to-string ()
  (subseq *read-buffer* 0 *ouch-ptr*))

;;;; READ-PRESERVING-WHITESPACE, READ-DELIMITED-LIST, and READ

;;; an alist for #=, used to keep track of objects with labels assigned that
;;; have been completely read. Each entry is (integer-tag gensym-tag value).
;;;
;;; KLUDGE: Should this really be an alist? It seems as though users
;;; could reasonably expect N log N performance for large datasets.
;;; On the other hand, it's probably very very seldom a problem in practice.
;;; On the third hand, it might be just as easy to use a hash table
;;; as an alist, so maybe we should. -- WHN 19991202
(defvar *sharp-equal-alist* ())

(declaim (special *standard-input*))

;;; READ-PRESERVING-WHITESPACE behaves just like READ, only it makes
;;; sure to leave terminating whitespace in the stream. (This is a
;;; COMMON-LISP exported symbol.)
(defun read-preserving-whitespace (&optional (stream *standard-input*)
					     (eof-error-p t)
					     (eof-value nil)
					     (recursivep nil))
  #!+sb-doc
  "Reads from stream and returns the object read, preserving the whitespace
   that followed the object."
  (cond
   (recursivep
    ;; a loop for repeating when a macro returns nothing
    (loop
      (let ((char (read-char stream eof-error-p *eof-object*)))
	(cond ((eofp char) (return eof-value))
	      ((whitespacep char))
	      (t
	       (let* ((macrofun (get-cmt-entry char *readtable*))
		      (result (multiple-value-list
			       (funcall macrofun stream char))))
		 ;; Repeat if macro returned nothing.
		 (if result (return (car result)))))))))
   (t
    (let ((*sharp-equal-alist* nil))
      (read-preserving-whitespace stream eof-error-p eof-value t)))))

;;; Return NIL or a list with one thing, depending.
;;;
;;; for functions that want comments to return so that they can look
;;; past them. Assumes char is not whitespace.
(defun read-maybe-nothing (stream char)
  (let ((retval (multiple-value-list
		 (funcall (get-cmt-entry char *readtable*) stream char))))
    (if retval (rplacd retval nil))))

(defun read (&optional (stream *standard-input*) (eof-error-p t)
		       (eof-value ()) (recursivep ()))
  #!+sb-doc
  "Reads in the next object in the stream, which defaults to
   *standard-input*. For details see the I/O chapter of
   the manual."
  (prog1
      (read-preserving-whitespace stream eof-error-p eof-value recursivep)
    (let ((whitechar (read-char stream nil *eof-object*)))
      (if (and (not (eofp whitechar))
	       (or (not (whitespacep whitechar))
		   recursivep))
	  (unread-char whitechar stream)))))

;;; (This is a COMMON-LISP exported symbol.)
(defun read-delimited-list (endchar &optional
				    (input-stream *standard-input*)
				    recursive-p)
  #!+sb-doc
  "Reads objects from input-stream until the next character after an
   object's representation is endchar. A list of those objects read
   is returned."
  (declare (ignore recursive-p))
  (do ((char (flush-whitespace input-stream)
	     (flush-whitespace input-stream))
       (retlist ()))
      ((char= char endchar) (nreverse retlist))
    (setq retlist (nconc (read-maybe-nothing input-stream char) retlist))))

;;;; basic readmacro definitions
;;;;
;;;; Large, hairy subsets of readmacro definitions (backquotes and sharp
;;;; macros) are not here, but in their own source files.

(defun read-quote (stream ignore)
  (declare (ignore ignore))
  (list 'quote (read stream t nil t)))

(defun read-comment (stream ignore)
  (declare (ignore ignore))
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-char stream
	  (do ((char (fast-read-char nil nil)
		     (fast-read-char nil nil)))
	      ((or (not char) (char= char #\newline))
	       (done-with-fast-read-char))))
	;; FUNDAMENTAL-STREAM
	(do ((char (stream-read-char stream) (stream-read-char stream)))
	    ((or (eq char :eof) (char= char #\newline))))))
  ;; Don't return anything.
  (values))

(defun read-list (stream ignore)
  (declare (ignore ignore))
  (let* ((thelist (list nil))
	 (listtail thelist))
    (do ((firstchar (flush-whitespace stream) (flush-whitespace stream)))
	((char= firstchar #\) ) (cdr thelist))
      (when (char= firstchar #\.)
	    (let ((nextchar (read-char stream t)))
	      (cond ((token-delimiterp nextchar)
		     (cond ((eq listtail thelist)
			    (%reader-error
			     stream
			     "Nothing appears before . in list."))
			   ((whitespacep nextchar)
			    (setq nextchar (flush-whitespace stream))))
		     (rplacd listtail
			     ;; Return list containing last thing.
			     (car (read-after-dot stream nextchar)))
		     (return (cdr thelist)))
		    ;; Put back NEXTCHAR so that we can read it normally.
		    (t (unread-char nextchar stream)))))
      ;; Next thing is not an isolated dot.
      (let ((listobj (read-maybe-nothing stream firstchar)))
	;; allows the possibility that a comment was read
	(when listobj
	      (rplacd listtail listobj)
	      (setq listtail listobj))))))

(defun read-after-dot (stream firstchar)
  ;; FIRSTCHAR is non-whitespace!
  (let ((lastobj ()))
    (do ((char firstchar (flush-whitespace stream)))
	((char= char #\) )
	 (%reader-error stream "Nothing appears after . in list."))
      ;; See whether there's something there.
      (setq lastobj (read-maybe-nothing stream char))
      (when lastobj (return t)))
    ;; At least one thing appears after the dot.
    ;; Check for more than one thing following dot.
    (do ((lastchar (flush-whitespace stream)
		   (flush-whitespace stream)))
	((char= lastchar #\) ) lastobj)	;success!
      ;; Try reading virtual whitespace.
      (if (read-maybe-nothing stream lastchar)
	  (%reader-error stream "More than one object follows . in list.")))))

(defun read-string (stream closech)
  ;; This accumulates chars until it sees same char that invoked it.
  ;; For a very long string, this could end up bloating the read buffer.
  (reset-read-buffer)
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-char stream
	  (do ((char (fast-read-char t) (fast-read-char t)))
	      ((char= char closech)
	       (done-with-fast-read-char))
	    (if (escapep char) (setq char (fast-read-char t)))
	    (ouch-read-buffer char)))
	;; FUNDAMENTAL-STREAM
	(do ((char (stream-read-char stream) (stream-read-char stream)))
	    ((or (eq char :eof) (char= char closech))
	     (if (eq char :eof)
		 (error 'end-of-file :stream stream)))
	  (when (escapep char)
	    (setq char (stream-read-char stream))
	    (if (eq char :eof)
		(error 'end-of-file :stream stream)))
	  (ouch-read-buffer char))))
  (read-buffer-to-string))

(defun read-right-paren (stream ignore)
  (declare (ignore ignore))
  (%reader-error stream "unmatched close parenthesis"))

;;; Read from the stream up to the next delimiter. Leave the resulting
;;; token in *READ-BUFFER*, and return two values:
;;; -- a list of the escaped character positions, and
;;; -- The position of the first package delimiter (or NIL).
(defun internal-read-extended-token (stream firstchar escape-firstchar)
  (reset-read-buffer)
  (let ((escapes '()))
    (when escape-firstchar
      (push *ouch-ptr* escapes)
      (ouch-read-buffer firstchar)
      (setq firstchar (read-char stream nil *eof-object*)))
  (do ((char firstchar (read-char stream nil *eof-object*))
       (colon nil))
      ((cond ((eofp char) t)
	     ((token-delimiterp char)
	      (unread-char char stream)
	      t)
	     (t nil))
       (values escapes colon))
    (cond ((escapep char)
	   ;; It can't be a number, even if it's 1\23.
	   ;; Read next char here, so it won't be casified.
	   (push *ouch-ptr* escapes)
	   (let ((nextchar (read-char stream nil *eof-object*)))
	     (if (eofp nextchar)
		 (reader-eof-error stream "after escape character")
		 (ouch-read-buffer nextchar))))
	  ((multiple-escape-p char)
	   ;; Read to next multiple-escape, escaping single chars
	   ;; along the way.
	   (loop
	     (let ((ch (read-char stream nil *eof-object*)))
	       (cond
		((eofp ch)
		 (reader-eof-error stream "inside extended token"))
		((multiple-escape-p ch) (return))
		((escapep ch)
		 (let ((nextchar (read-char stream nil *eof-object*)))
		   (cond ((eofp nextchar)
			  (reader-eof-error stream "after escape character"))
			 (t
			  (push *ouch-ptr* escapes)
			  (ouch-read-buffer nextchar)))))
		(t
		 (push *ouch-ptr* escapes)
		 (ouch-read-buffer ch))))))
	  (t
	   (when (and (constituentp char)
			(eql (get-secondary-attribute char)
                             +char-attr-package-delimiter+)
		      (not colon))
	     (setq colon *ouch-ptr*))
	   (ouch-read-buffer char))))))

;;;; character classes

;;; Return the character class for CHAR.
(defmacro char-class (char attable)
  `(let ((att (aref ,attable (char-code ,char))))
     (declare (fixnum att))
     (if (<= att +char-attr-terminating-macro+)
	 +char-attr-delimiter+
	 att)))

;;; Return the character class for CHAR, which might be part of a
;;; rational number.
(defmacro char-class2 (char attable)
  `(let ((att (aref ,attable (char-code ,char))))
     (declare (fixnum att))
     (if (<= att +char-attr-terminating-macro+)
	 +char-attr-delimiter+
	 (if (digit-char-p ,char *read-base*)
	     +char-attr-constituent-digit+
	     (if (= att +char-attr-constituent-digit+)
		 +char-attr-constituent+
		 att)))))

;;; Return the character class for a char which might be part of a
;;; rational or floating number. (Assume that it is a digit if it
;;; could be.)
(defmacro char-class3 (char attable)
  `(let ((att (aref ,attable (char-code ,char))))
     (declare (fixnum att))
     (if possibly-rational
	 (setq possibly-rational
	       (or (digit-char-p ,char *read-base*)
		   (= att +char-attr-constituent-slash+))))
     (if possibly-float
	 (setq possibly-float
	       (or (digit-char-p ,char 10)
		   (= att +char-attr-constituent-dot+))))
     (if (<= att +char-attr-terminating-macro+)
	 +char-attr-delimiter+
	 (if (digit-char-p ,char (max *read-base* 10))
	     (if (digit-char-p ,char *read-base*)
		 +char-attr-constituent-digit+
		 +char-attr-constituent+)
	     att))))

;;;; token fetching

(defvar *read-suppress* nil
  #!+sb-doc
  "Suppresses most interpreting of the reader when T")

(defvar *read-base* 10
  #!+sb-doc
  "The radix that Lisp reads numbers in.")
(declaim (type (integer 2 36) *read-base*))

;;; Modify the read buffer according to READTABLE-CASE, ignoring
;;; ESCAPES. ESCAPES is a list of the escaped indices, in reverse
;;; order.
(defun casify-read-buffer (escapes)
  (let ((case (readtable-case *readtable*)))
    (cond
     ((and (null escapes) (eq case :upcase))
      (dotimes (i *ouch-ptr*)
	(setf (schar *read-buffer* i)
	      (char-upcase (schar *read-buffer* i)))))
     ((eq case :preserve))
     (t
      (macrolet ((skip-esc (&body body)
		   `(do ((i (1- *ouch-ptr*) (1- i))
			 (escapes escapes))
			((minusp i))
		      (declare (fixnum i))
		      (when (or (null escapes)
				(let ((esc (first escapes)))
				  (declare (fixnum esc))
				  (cond ((< esc i) t)
					(t
					 (aver (= esc i))
					 (pop escapes)
					 nil))))
			(let ((ch (schar *read-buffer* i)))
			  ,@body)))))
	(flet ((lower-em ()
		 (skip-esc (setf (schar *read-buffer* i) (char-downcase ch))))
	       (raise-em ()
		 (skip-esc (setf (schar *read-buffer* i) (char-upcase ch)))))
	  (ecase case
	    (:upcase (raise-em))
	    (:downcase (lower-em))
	    (:invert
	     (let ((all-upper t)
		   (all-lower t))
	       (skip-esc
		 (when (both-case-p ch)
		   (if (upper-case-p ch)
		       (setq all-lower nil)
		       (setq all-upper nil))))
	       (cond (all-lower (raise-em))
		     (all-upper (lower-em))))))))))))

(defun read-token (stream firstchar)
  #!+sb-doc
  "This function is just an fsm that recognizes numbers and symbols."
  ;; Check explicitly whether FIRSTCHAR has an entry for
  ;; NON-TERMINATING in CHARACTER-ATTRIBUTE-TABLE and
  ;; READ-DOT-NUMBER-SYMBOL in CMT. Report an error if these are
  ;; violated. (If we called this, we want something that is a
  ;; legitimate token!) Read in the longest possible string satisfying
  ;; the Backus-Naur form for "unqualified-token". Leave the result in
  ;; the *READ-BUFFER*. Return next char after token (last char read).
  (when *read-suppress*
    (internal-read-extended-token stream firstchar nil)
    (return-from read-token nil))
  (let ((attribute-table (character-attribute-table *readtable*))
	(package-designator nil)
	(colons 0)
	(possibly-rational t)
	(possibly-float t)
	(escapes ()))
    (reset-read-buffer)
    (prog ((char firstchar))
      (case (char-class3 char attribute-table)
	(#.+char-attr-constituent-sign+ (go SIGN))
	(#.+char-attr-constituent-digit+ (go LEFTDIGIT))
	(#.+char-attr-constituent-dot+ (go FRONTDOT))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	;; can't have eof, whitespace, or terminating macro as first char!
	(t (go SYMBOL)))
     SIGN ; saw "sign"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (setq possibly-rational t
	    possibly-float t)
      (case (char-class3 char attribute-table)
	(#.+char-attr-constituent-digit+ (go LEFTDIGIT))
	(#.+char-attr-constituent-dot+ (go SIGNDOT))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))	
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(t (go SYMBOL)))
     LEFTDIGIT ; saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-integer)))
      (case (char-class3 char attribute-table)
	(#.+char-attr-constituent-digit+ (go LEFTDIGIT))
	(#.+char-attr-constituent-dot+ (if possibly-float
					   (go MIDDLEDOT)
					   (go SYMBOL)))
	(#.+char-attr-constituent-expt+ (go EXPONENT))
	(#.+char-attr-constituent-slash+ (if possibly-rational
					     (go RATIO)
					     (go SYMBOL)))
	(#.+char-attr-delimiter+ (unread-char char stream)
				 (return (make-integer)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     MIDDLEDOT ; saw "[sign] {digit}+ dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (let ((*read-base* 10))
			     (make-integer))))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
	(#.+char-attr-constituent-expt+ (go EXPONENT))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (return (let ((*read-base* 10))
		   (make-integer))))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     RIGHTDIGIT ; saw "[sign] {digit}* dot {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float)))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
	(#.+char-attr-constituent-expt+ (go EXPONENT))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (return (make-float)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     SIGNDOT ; saw "[sign] dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(t (go SYMBOL)))
     FRONTDOT ; saw "dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "dot context error"))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
	(#.+char-attr-constituent-dot+ (go DOTS))
	(#.+char-attr-delimiter+  (%reader-error stream "dot context error"))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     EXPONENT
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-sign+ (go EXPTSIGN))
	(#.+char-attr-constituent-digit+ (go EXPTDIGIT))
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     EXPTSIGN ; got to EXPONENT, and saw a sign character
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-digit+ (go EXPTDIGIT))
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     EXPTDIGIT ; got to EXPONENT, saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float)))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-digit+ (go EXPTDIGIT))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (return (make-float)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     RATIO ; saw "[sign] {digit}+ slash"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class2 char attribute-table)
	(#.+char-attr-constituent-digit+ (go RATIODIGIT))
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     RATIODIGIT ; saw "[sign] {digit}+ slash {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-ratio)))
      (case (char-class2 char attribute-table)
	(#.+char-attr-constituent-digit+ (go RATIODIGIT))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (return (make-ratio)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     DOTS ; saw "dot {dot}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "too many dots"))
      (case (char-class char attribute-table)
	(#.+char-attr-constituent-dot+ (go DOTS))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (%reader-error stream "too many dots"))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     SYMBOL ; not a dot, dots, or number
      (let ((stream (in-synonym-of stream)))
	(if (lisp-stream-p stream)
	    (prepare-for-fast-read-char stream
	      (prog ()
	       SYMBOL-LOOP
	       (ouch-read-buffer char)
	       (setq char (fast-read-char nil nil))
	       (unless char (go RETURN-SYMBOL))
	       (case (char-class char attribute-table)
		 (#.+char-attr-escape+ (done-with-fast-read-char)
				       (go ESCAPE))
		 (#.+char-attr-delimiter+ (done-with-fast-read-char)
					  (unread-char char stream)
					  (go RETURN-SYMBOL))
		 (#.+char-attr-multiple-escape+ (done-with-fast-read-char)
						(go MULT-ESCAPE))
		 (#.+char-attr-package-delimiter+ (done-with-fast-read-char)
						  (go COLON))
		 (t (go SYMBOL-LOOP)))))
	    ;; fundamental-stream
	    (prog ()
	     SYMBOL-LOOP
	     (ouch-read-buffer char)
	     (setq char (stream-read-char stream))
	     (when (eq char :eof) (go RETURN-SYMBOL))
	     (case (char-class char attribute-table)
	       (#.+char-attr-escape+ (go ESCAPE))
	       (#.+char-attr-delimiter+ (stream-unread-char stream char)
			    (go RETURN-SYMBOL))
	       (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	       (#.+char-attr-package-delimiter+ (go COLON))
	       (t (go SYMBOL-LOOP))))))
     ESCAPE ; saw an escape
      ;; Don't put the escape in the read buffer.
      ;; READ-NEXT CHAR, put in buffer (no case conversion).
      (let ((nextchar (read-char stream nil nil)))
	(unless nextchar
	  (reader-eof-error stream "after escape character"))
	(push *ouch-ptr* escapes)
	(ouch-read-buffer nextchar))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
      MULT-ESCAPE
      (do ((char (read-char stream t) (read-char stream t)))
	  ((multiple-escape-p char))
	(if (escapep char) (setq char (read-char stream t)))
	(push *ouch-ptr* escapes)
	(ouch-read-buffer char))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-table)
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
      COLON
      (casify-read-buffer escapes)
      (unless (zerop colons)
	(%reader-error stream "too many colons in ~S"
		      (read-buffer-to-string)))
      (setq colons 1)
      (setq package-designator
	    (if (plusp *ouch-ptr*)
		;; FIXME: It seems inefficient to cons up a package
		;; designator string every time we read a symbol with an
		;; explicit package prefix. Perhaps we could implement
		;; a FIND-PACKAGE* function analogous to INTERN*
		;; and friends?
		(read-buffer-to-string)
		*keyword-package*))
      (reset-read-buffer)
      (setq escapes ())
      (setq char (read-char stream nil nil))
      (unless char (reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-table)
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (%reader-error stream
			"illegal terminating character after a colon: ~S"
			char))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go INTERN))
	(t (go SYMBOL)))
      INTERN
      (setq colons 2)
      (setq char (read-char stream nil nil))
      (unless char
	(reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-table)
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (%reader-error stream
			"illegal terminating character after a colon: ~S"
			char))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+
	 (%reader-error stream
			"too many colons after ~S name"
			package-designator))
	(t (go SYMBOL)))
      RETURN-SYMBOL
      (casify-read-buffer escapes)
      (let ((found (if package-designator
		       (find-package package-designator)
		       (sane-package))))
	(unless found
	  (error 'reader-package-error :stream stream
		 :format-arguments (list package-designator)
		 :format-control "package ~S not found"))

	(if (or (zerop colons) (= colons 2) (eq found *keyword-package*))
	    (return (intern* *read-buffer* *ouch-ptr* found))
	    (multiple-value-bind (symbol test)
		(find-symbol* *read-buffer* *ouch-ptr* found)
	      (when (eq test :external) (return symbol))
	      (let ((name (read-buffer-to-string)))
		(with-simple-restart (continue "Use symbol anyway.")
		  (error 'reader-package-error :stream stream
			 :format-arguments (list name (package-name found))
			 :format-control
			 (if test
			     "The symbol ~S is not external in the ~A package."
			     "Symbol ~S not found in the ~A package.")))
		(return (intern name found)))))))))

;;; for semi-external use:
;;;
;;; For semi-external use: Return 3 values: the string for the token,
;;; a flag for whether there was an escape char, and the position of
;;; any package delimiter.
(defun read-extended-token (stream &optional (*readtable* *readtable*))
  (let ((first-char (read-char stream nil nil t)))
    (cond (first-char
	   (multiple-value-bind (escapes colon)
               (internal-read-extended-token stream first-char nil)
	     (casify-read-buffer escapes)
	     (values (read-buffer-to-string) (not (null escapes)) colon)))
	  (t
	   (values "" nil nil)))))

;;; for semi-external use:
;;;
;;; Read an extended token with the first character escaped. Return
;;; the string for the token.
(defun read-extended-token-escaped (stream &optional (*readtable* *readtable*))
  (let ((first-char (read-char stream nil nil)))
    (cond (first-char
            (let ((escapes (internal-read-extended-token stream first-char t)))
              (casify-read-buffer escapes)
              (read-buffer-to-string)))
          (t
            (reader-eof-error stream "after escape")))))

;;;; number-reading functions

(defmacro digit* nil
  `(do ((ch char (inch-read-buffer)))
       ((or (eofp ch) (not (digit-char-p ch))) (setq char ch))
     ;; Report if at least one digit is seen.
     (setq one-digit t)))

(defmacro exponent-letterp (letter)
  `(memq ,letter '(#\E #\S #\F #\L #\D #\e #\s #\f #\l #\d)))

;;; FIXME: It would be cleaner to have these generated automatically
;;; by compile-time code instead of having them hand-created like
;;; this. The !COLD-INIT-INTEGER-READER code below should be resurrected
;;; and tested.
(defvar *integer-reader-safe-digits*
  #(nil nil
    26 17 13 11 10 9 8 8 8 7 7 7 7 6 6 6 6 6 6 6 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
  #!+sb-doc
  "the mapping of base to 'safe' number of digits to read for a fixnum")
(defvar *integer-reader-base-power*
  #(nil nil
    67108864 129140163 67108864 48828125 60466176 40353607
    16777216 43046721 100000000 19487171 35831808 62748517 105413504 11390625
    16777216 24137569 34012224 47045881 64000000 85766121 113379904 6436343
    7962624 9765625 11881376 14348907 17210368 20511149 24300000 28629151
    33554432 39135393 45435424 52521875 60466176)
  #!+sb-doc
  "the largest fixnum power of the base for MAKE-INTEGER")
(declaim (simple-vector *integer-reader-safe-digits*
			*integer-reader-base-power*))
#|
(defun !cold-init-integer-reader ()
  (do ((base 2 (1+ base)))
      ((> base 36))
    (let ((digits
	  (do ((fix (truncate most-positive-fixnum base)
		    (truncate fix base))
	       (digits 0 (1+ digits)))
	      ((zerop fix) digits))))
      (setf (aref *integer-reader-safe-digits* base)
	    digits
	    (aref *integer-reader-base-power* base)
	    (expt base digits)))))
|#

(defun make-integer ()
  #!+sb-doc
  "Minimizes bignum-fixnum multiplies by reading a 'safe' number of digits,
  then multiplying by a power of the base and adding."
  (let* ((base *read-base*)
	 (digits-per (aref *integer-reader-safe-digits* base))
	 (base-power (aref *integer-reader-base-power* base))
	 (negativep nil)
	 (number 0))
    (declare (type index digits-per base-power))
    (read-unwind-read-buffer)
    (let ((char (inch-read-buffer)))
      (cond ((char= char #\-)
	     (setq negativep t))
	    ((char= char #\+))
	    (t (unread-buffer))))
    (loop
     (let ((num 0))
       (declare (type index num))
       (dotimes (digit digits-per)
	 (let* ((ch (inch-read-buffer)))
	   (cond ((or (eofp ch) (char= ch #\.))
		  (return-from make-integer
			       (let ((res
				      (if (zerop number) num
					  (+ num (* number
						    (expt base digit))))))
				 (if negativep (- res) res))))
		 (t (setq num (+ (digit-char-p ch base)
				 (the index (* num base))))))))
       (setq number (+ num (* number base-power)))))))

(defun make-float ()
  ;; Assume that the contents of *read-buffer* are a legal float, with nothing
  ;; else after it.
  (read-unwind-read-buffer)
  (let ((negative-fraction nil)
	(number 0)
	(divisor 1)
	(negative-exponent nil)
	(exponent 0)
	(float-char ())
	(char (inch-read-buffer)))
    (if (cond ((char= char #\+) t)
	      ((char= char #\-) (setq negative-fraction t)))
	;; Flush it.
	(setq char (inch-read-buffer)))
    ;; Read digits before the dot.
    (do* ((ch char (inch-read-buffer))
	  (dig (digit-char-p ch) (digit-char-p ch)))
	 ((not dig) (setq char ch))
      (setq number (+ (* number 10) dig)))
    ;; Deal with the dot, if it's there.
    (when (char= char #\.)
      (setq char (inch-read-buffer))
      ;; Read digits after the dot.
      (do* ((ch char (inch-read-buffer))
	    (dig (and (not (eofp ch)) (digit-char-p ch))
		 (and (not (eofp ch)) (digit-char-p ch))))
	   ((not dig) (setq char ch))
	(setq divisor (* divisor 10))
	(setq number (+ (* number 10) dig))))
    ;; Is there an exponent letter?
    (cond ((eofp char)
	   ;; If not, we've read the whole number.
	   (let ((num (make-float-aux number divisor
				      *read-default-float-format*)))
	     (return-from make-float (if negative-fraction (- num) num))))
	  ((exponent-letterp char)
	   (setq float-char char)
	   ;; Build exponent.
	   (setq char (inch-read-buffer))
	   ;; Check leading sign.
	   (if (cond ((char= char #\+) t)
		     ((char= char #\-) (setq negative-exponent t)))
	       ;; Flush sign.
	       (setq char (inch-read-buffer)))
	   ;; Read digits for exponent.
	   (do* ((ch char (inch-read-buffer))
		 (dig (and (not (eofp ch)) (digit-char-p ch))
		      (and (not (eofp ch)) (digit-char-p ch))))
		((not dig)
		 (setq exponent (if negative-exponent (- exponent) exponent)))
	     (setq exponent (+ (* exponent 10) dig)))
	   ;; Generate and return the float, depending on float-char:
	   (let* ((float-format (case (char-upcase float-char)
				  (#\E *read-default-float-format*)
				  (#\S 'short-float)
				  (#\F 'single-float)
				  (#\D 'double-float)
				  (#\L 'long-float)))
		  num)
	     ;; toy@rtp.ericsson.se: We need to watch out if the
	     ;; exponent is too small or too large. We add enough to
	     ;; EXPONENT to make it within range and scale NUMBER
	     ;; appropriately. This should avoid any unnecessary
	     ;; underflow or overflow problems.
	     (multiple-value-bind (min-expo max-expo)
		 ;; FIXME: These #. forms are broken w.r.t.
		 ;; cross-compilation portability. Maybe expressions
		 ;; like
		 ;;   (LOG SB!XC:MOST-POSITIVE-SHORT-FLOAT 10s0)
		 ;; could be used instead? Or perhaps some sort of
		 ;; load-time-form magic?
		 (case float-format
		   (short-float
		    (values
		     #.(log least-positive-normalized-short-float 10s0)
		     #.(log most-positive-short-float 10s0)))
		   (single-float
		    (values
		     #.(log least-positive-normalized-single-float 10f0)
		     #.(log most-positive-single-float 10f0)))
		   (double-float
		    (values
		     #.(log least-positive-normalized-double-float 10d0)
		     #.(log most-positive-double-float 10d0)))
		   (long-float
		    (values
		     #.(log least-positive-normalized-long-float 10L0)
		     #.(log most-positive-long-float 10L0))))
	       (let ((correction (cond ((<= exponent min-expo)
					(ceiling (- min-expo exponent)))
				       ((>= exponent max-expo)
					(floor (- max-expo exponent)))
				       (t
					0))))
		 (incf exponent correction)
		 (setf number (/ number (expt 10 correction)))
		 (setq num (make-float-aux number divisor float-format))
		 (setq num (* num (expt 10 exponent)))
		 (return-from make-float (if negative-fraction
					     (- num)
					     num))))))
	  ;; should never happen:	
	  (t (error "internal error in floating point reader")))))

(defun make-float-aux (number divisor float-format)
  (coerce (/ number divisor) float-format))

(defun make-ratio ()
  ;; Assume *READ-BUFFER* contains a legal ratio. Build the number from
  ;; the string.
  ;;
  ;; Look for optional "+" or "-".
  (let ((numerator 0) (denominator 0) (char ()) (negative-number nil))
    (read-unwind-read-buffer)
    (setq char (inch-read-buffer))
    (cond ((char= char #\+)
	   (setq char (inch-read-buffer)))
	  ((char= char #\-)
	   (setq char (inch-read-buffer))
	   (setq negative-number t)))
    ;; Get numerator.
    (do* ((ch char (inch-read-buffer))
	  (dig (digit-char-p ch *read-base*)
	       (digit-char-p ch *read-base*)))
	 ((not dig))
	 (setq numerator (+ (* numerator *read-base*) dig)))
    ;; Get denominator.
    (do* ((ch (inch-read-buffer) (inch-read-buffer))
	  (dig ()))
	 ((or (eofp ch) (not (setq dig (digit-char-p ch *read-base*)))))
	 (setq denominator (+ (* denominator *read-base*) dig)))
    (let ((num (/ numerator denominator)))
      (if negative-number (- num) num))))

;;;; cruft for dispatch macros

(defun make-char-dispatch-table ()
  (make-array char-code-limit :initial-element #'dispatch-char-error))

(defun dispatch-char-error (stream sub-char ignore)
  (declare (ignore ignore))
  (if *read-suppress*
      (values)
      (%reader-error stream "no dispatch function defined for ~S" sub-char)))

(defun make-dispatch-macro-character (char &optional
					   (non-terminating-p nil)
					   (rt *readtable*))
  #!+sb-doc
  "Cause CHAR to become a dispatching macro character in readtable (which
   defaults to the current readtable). If NON-TERMINATING-P, the char will
   be non-terminating."
  (set-macro-character char #'read-dispatch-char non-terminating-p rt)
  (let* ((dalist (dispatch-tables rt))
	 (dtable (cdr (find char dalist :test #'char= :key #'car))))
    (cond (dtable
	   (error "The dispatch character ~S already exists." char))
	  (t
	   (setf (dispatch-tables rt)
		 (push (cons char (make-char-dispatch-table)) dalist)))))
  t)

(defun set-dispatch-macro-character (disp-char sub-char function
                                               &optional (rt *readtable*))
  #!+sb-doc
  "Cause FUNCTION to be called whenever the reader reads DISP-CHAR
   followed by SUB-CHAR."
  ;; Get the dispatch char for macro (error if not there), diddle
  ;; entry for sub-char.
  (when (digit-char-p sub-char)
    (error "SUB-CHAR must not be a decimal digit: ~S" sub-char))
  (let* ((sub-char (char-upcase sub-char))
	 (dpair (find disp-char (dispatch-tables rt)
		      :test #'char= :key #'car)))
    (if dpair
	(setf (elt (the simple-vector (cdr dpair))
		   (char-code sub-char))
	      (coerce function 'function))
	(error "~S is not a dispatch char." disp-char))))

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (rt *readtable*))
  #!+sb-doc
  "Returns the macro character function for sub-char under disp-char
   or nil if there is no associated function."
  (unless (digit-char-p sub-char)
    (let* ((sub-char (char-upcase sub-char))
	   (rt (or rt *standard-readtable*))
	   (dpair (find disp-char (dispatch-tables rt)
			:test #'char= :key #'car)))
      (if dpair
	  (elt (the simple-vector (cdr dpair))
	       (char-code sub-char))
	  (error "~S is not a dispatch char." disp-char)))))

(defun read-dispatch-char (stream char)
  ;; Read some digits.
  (let ((numargp nil)
	(numarg 0)
	(sub-char ()))
    (do* ((ch (read-char stream nil *eof-object*)
	      (read-char stream nil *eof-object*))
	  (dig ()))
	 ((or (eofp ch)
	      (not (setq dig (digit-char-p ch))))
	  ;; Take care of the extra char.
	  (if (eofp ch)
	      (reader-eof-error stream "inside dispatch character")
	      (setq sub-char (char-upcase ch))))
      (setq numargp t)
      (setq numarg (+ (* numarg 10) dig)))
    ;; Look up the function and call it.
    (let ((dpair (find char (dispatch-tables *readtable*)
		       :test #'char= :key #'car)))
      (if dpair
	  (funcall (the function
			(elt (the simple-vector (cdr dpair))
			     (char-code sub-char)))
		   stream sub-char (if numargp numarg nil))
	  (%reader-error stream "no dispatch table for dispatch char")))))

;;;; READ-FROM-STRING

;;; FIXME: Is it really worth keeping this pool?
(defvar *read-from-string-spares* ()
  #!+sb-doc
  "A resource of string streams for Read-From-String.")

(defun read-from-string (string &optional eof-error-p eof-value
				&key (start 0) end
				preserve-whitespace)
  #!+sb-doc
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (declare (string string))
  (with-array-data ((string string)
		    (start start)
		    (end (or end (length string))))
    (unless *read-from-string-spares*
      (push (internal-make-string-input-stream "" 0 0)
	    *read-from-string-spares*))
    (let ((stream (pop *read-from-string-spares*)))
      (setf (string-input-stream-string stream) string)
      (setf (string-input-stream-current stream) start)
      (setf (string-input-stream-end stream) end)
      (unwind-protect
	  (values (if preserve-whitespace
		      (read-preserving-whitespace stream eof-error-p eof-value)
		      (read stream eof-error-p eof-value))
		  (string-input-stream-current stream))
	(push stream *read-from-string-spares*)))))

;;;; PARSE-INTEGER

(defun parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  #!+sb-doc
  "Examine the substring of string delimited by start and end
  (default to the beginning and end of the string)  It skips over
  whitespace characters and then tries to parse an integer. The
  radix parameter must be between 2 and 36."
  (with-array-data ((string string)
		    (start start)
		    (end (or end (length string))))
    (let ((index (do ((i start (1+ i)))
		     ((= i end)
		      (if junk-allowed
			  (return-from parse-integer (values nil end))
			  (error "no non-whitespace characters in number")))
		   (declare (fixnum i))
		   (unless (whitespacep (char string i)) (return i))))
	  (minusp nil)
	  (found-digit nil)
	  (result 0))
      (declare (fixnum index))
      (let ((char (char string index)))
	(cond ((char= char #\-)
	       (setq minusp t)
	       (incf index))
	      ((char= char #\+)
	       (incf index))))
      (loop
	(when (= index end) (return nil))
	(let* ((char (char string index))
	       (weight (digit-char-p char radix)))
	  (cond (weight
		 (setq result (+ weight (* result radix))
		       found-digit t))
		(junk-allowed (return nil))
		((whitespacep char)
		 (do ((jndex (1+ index) (1+ jndex)))
		     ((= jndex end))
		   (declare (fixnum jndex))
		   (unless (whitespacep (char string jndex))
		     (error "junk in string ~S" string)))
		 (return nil))
		(t
		 (error "junk in string ~S" string))))
	(incf index))
      (values
       (if found-digit
	   (if minusp (- result) result)
	   (if junk-allowed
	       nil
	       (error "no digits in string ~S" string)))
       index))))

;;;; reader initialization code

(defun !reader-cold-init ()
  (!cold-init-read-buffer)
  (!cold-init-secondary-attribute-table)
  (!cold-init-standard-readtable)
  ;; FIXME: This was commented out, but should probably be restored.
  #+nil (!cold-init-integer-reader))

(def!method print-object ((readtable readtable) stream)
  (print-unreadable-object (readtable stream :identity t :type t)))
