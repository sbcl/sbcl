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
  `(if (typep ,char 'base-char)
       (elt (character-attribute-array ,rt) (char-code ,char))
       (gethash ,char (character-attribute-hash-table ,rt) +char-attr-constituent+)))

(defun set-cat-entry (char newvalue &optional (rt *readtable*))
  (if (typep char 'base-char)
      (setf (elt (character-attribute-array rt) (char-code char)) newvalue)
      ;; FIXME: could REMHASH if we're setting to
      ;; +CHAR-ATTR-CONSTITUENT+
      (setf (gethash char (character-attribute-hash-table rt)) newvalue)))

;;; the value actually stored in the character macro table. As per
;;; ANSI #'GET-MACRO-CHARACTER and #'SET-MACRO-CHARACTER, this can
;;; be either a function or NIL.
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro get-raw-cmt-entry (char readtable)
    `(if (typep ,char 'base-char)
         (svref (character-macro-array ,readtable) (char-code ,char))
         ;; Note: DEFAULT here is NIL, not #'UNDEFINED-MACRO-CHAR, so
         ;; that everything above the base-char range is a non-macro
         ;; constituent by default.
         (gethash ,char (character-macro-hash-table ,readtable) nil))))

;;; the value represented by whatever is stored in the character macro
;;; table. As per ANSI #'GET-MACRO-CHARACTER and #'SET-MACRO-CHARACTER,
;;; a function value represents itself, and a NIL value represents the
;;; default behavior.
(defun get-coerced-cmt-entry (char readtable)
  (the function 
    (or (get-raw-cmt-entry char readtable)
	#'read-token)))

(defun set-cmt-entry (char new-value-designator &optional (rt *readtable*))
  (if (typep char 'base-char)
      (setf (svref (character-macro-array rt) (char-code char))
            (and new-value-designator
                 (%coerce-callable-to-fun new-value-designator)))
      (setf (gethash char (character-macro-hash-table rt))
	(and new-value-designator
                 (%coerce-callable-to-fun new-value-designator)))))

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
	(make-array base-char-code-limit :element-type '(unsigned-byte 8)
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

(defun shallow-replace/eql-hash-table (to from)
  (maphash (lambda (k v) (setf (gethash k to) v)) from))

(defun copy-readtable (&optional (from-readtable *readtable*)
				 to-readtable)
  (let ((really-from-readtable (or from-readtable *standard-readtable*))
        (really-to-readtable (or to-readtable (make-readtable))))
    (replace (character-attribute-array really-to-readtable)
	     (character-attribute-array really-from-readtable))
    (shallow-replace/eql-hash-table
     (character-attribute-hash-table really-to-readtable)
     (character-attribute-hash-table really-from-readtable))
    (replace (character-macro-array really-to-readtable)
	     (character-macro-array really-from-readtable))
    (shallow-replace/eql-hash-table
     (character-macro-hash-table really-to-readtable)
     (character-macro-hash-table really-from-readtable))
    (setf (dispatch-tables really-to-readtable)
	  (mapcar (lambda (pair)
                    (cons (car pair)
                          (let ((table (make-hash-table)))
                            (shallow-replace/eql-hash-table table (cdr pair))
                            table)))
		  (dispatch-tables really-from-readtable)))
    (setf (readtable-case really-to-readtable)
	  (readtable-case really-from-readtable))
    really-to-readtable))

(defun set-syntax-from-char (to-char from-char &optional
				     (to-readtable *readtable*)
				     (from-readtable ()))
  #!+sb-doc
  "Causes the syntax of TO-CHAR to be the same as FROM-CHAR in the
  optional readtable (defaults to the current readtable). The
  FROM-TABLE defaults to the standard Lisp readtable when NIL."
  (let ((really-from-readtable (or from-readtable *standard-readtable*)))
    ;; Copy FROM-CHAR entries to TO-CHAR entries, but make sure that if
    ;; FROM-CHAR is a constituent you don't copy non-movable secondary
    ;; attributes (constituent types), and that said attributes magically
    ;; appear if you transform a non-constituent to a constituent.
    (let ((att (get-cat-entry from-char really-from-readtable)))
      (if (constituentp from-char really-from-readtable)
	  (setq att (get-secondary-attribute to-char)))
      (set-cat-entry to-char att to-readtable)
      (set-cmt-entry to-char
		     (get-raw-cmt-entry from-char really-from-readtable)
		     to-readtable)))
  t)

(defun set-macro-character (char function &optional
				 (non-terminatingp nil)
				 (readtable *readtable*))
  #!+sb-doc
  "Causes CHAR to be a macro character which invokes FUNCTION when seen
   by the reader. The NON-TERMINATINGP flag can be used to make the macro
   character non-terminating, i.e. embeddable in a symbol name."
  (let ((designated-readtable (or readtable *standard-readtable*)))
    (set-cat-entry char
		   (if non-terminatingp
		       (get-secondary-attribute char)
		       +char-attr-terminating-macro+)
		   designated-readtable)
    (set-cmt-entry char function designated-readtable)
    t)) ; (ANSI-specified return value)

(defun get-macro-character (char &optional (readtable *readtable*))
  #!+sb-doc
  "Return the function associated with the specified CHAR which is a macro
  character, or NIL if there is no such function. As a second value, return
  T if CHAR is a macro character which is non-terminating, i.e. which can
  be embedded in a symbol name."
  (let* ((designated-readtable (or readtable *standard-readtable*))
	 ;; the first return value: a FUNCTION if CHAR is a macro
	 ;; character, or NIL otherwise
	 (fun-value (get-raw-cmt-entry char designated-readtable)))
    (values fun-value
	    ;; NON-TERMINATING-P return value:
	    (if fun-value
		(or (constituentp char)
		    (not (terminating-macrop char)))
		;; ANSI's definition of GET-MACRO-CHARACTER says this
		;; value is NIL when CHAR is not a macro character.
		;; I.e. this value means not just "non-terminating
		;; character?" but "non-terminating macro character?".
		nil))))

;;;; definitions to support internal programming conventions

(defmacro eofp (char)
  `(eq ,char *eof-object*))

(defun flush-whitespace (stream)
  ;; This flushes whitespace chars, returning the last char it read (a
  ;; non-white one). It always gets an error on end-of-file.
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
	(prepare-for-fast-read-char stream
	  (do ((attribute-array (character-attribute-array *readtable*))
               (attribute-hash-table
                (character-attribute-hash-table *readtable*))
	       (char (fast-read-char t) (fast-read-char t)))
	      ((/= (the fixnum
                     (if (typep char 'base-char)
                         (aref attribute-array (char-code char))
                         (gethash char attribute-hash-table +char-attr-constituent+)))
		   +char-attr-whitespace+)
	       (done-with-fast-read-char)
	       char)))
	;; CLOS stream
	(do ((attribute-array (character-attribute-array *readtable*))
             (attribute-hash-table
              (character-attribute-hash-table *readtable*))
	     (char (read-char stream nil :eof) (read-char stream nil :eof)))
	    ((or (eq char :eof)
		 (/= (the fixnum
                       (if (typep char 'base-char)
                           (aref attribute-array (char-code char))
                           (gethash char attribute-hash-table +char-attr-constituent+)))
		     +char-attr-whitespace+))
	     (if (eq char :eof)
		 (error 'end-of-file :stream stream)
		 char))))))

;;;; temporary initialization hack

(defun !cold-init-standard-readtable ()
  (setq *standard-readtable* (make-readtable))
  ;; All characters get boring defaults in MAKE-READTABLE. Now we
  ;; override the boring defaults on characters which need more
  ;; interesting behavior.
  (let ((*readtable* *standard-readtable*))

    (flet ((whitespaceify (char)
	     (set-cmt-entry char nil)
	     (set-cat-entry char +char-attr-whitespace+)))
      (whitespaceify (code-char tab-char-code))
      (whitespaceify #\linefeed)
      (whitespaceify #\space)
      (whitespaceify (code-char form-feed-char-code))
      (whitespaceify (code-char return-char-code)))

    (set-cat-entry #\\ +char-attr-escape+)
    (set-cmt-entry #\\ nil)

    ;; Easy macro-character definitions are in this source file.
    (set-macro-character #\" #'read-string)
    (set-macro-character #\' #'read-quote)
    (set-macro-character #\( #'read-list)
    (set-macro-character #\) #'read-right-paren)
    (set-macro-character #\; #'read-comment)
    ;; (The hairier macro-character definitions, for #\# and #\`, are
    ;; defined elsewhere, in their own source files.)

    ;; all constituents
    (do ((ichar 0 (1+ ichar))
	 (char))
	((= ichar base-char-code-limit))
      (setq char (code-char ichar))
      (when (constituentp char *standard-readtable*)
	(set-cat-entry char (get-secondary-attribute char))
	(set-cmt-entry char nil)))))

;;;; implementation of the read buffer

(defvar *read-buffer*)
(defvar *read-buffer-length*)
;;; FIXME: Is it really helpful to have *READ-BUFFER-LENGTH* be a
;;; separate variable instead of just calculating it on the fly as
;;; (LENGTH *READ-BUFFER*)?

(defvar *inch-ptr*)
(defvar *ouch-ptr*)

(declaim (type index *read-buffer-length* *inch-ptr* *ouch-ptr*))
(declaim (type (simple-array character (*)) *read-buffer*))

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
  "Read from STREAM and return the value read, preserving any whitespace
   that followed the object."
  (if recursivep
      ;; a loop for repeating when a macro returns nothing
      (loop
       (let ((char (read-char stream eof-error-p *eof-object*)))
         (cond ((eofp char) (return eof-value))
               ((whitespacep char))
               (t
                (let* ((macrofun (get-coerced-cmt-entry char *readtable*))
                       (result (multiple-value-list
                                (funcall macrofun stream char))))
                  ;; Repeat if macro returned nothing.
		  (when result 
                    (return (unless *read-suppress* (car result)))))))))
      (let ((*sharp-equal-alist* nil))
	(read-preserving-whitespace stream eof-error-p eof-value t))))

;;; Return NIL or a list with one thing, depending.
;;;
;;; for functions that want comments to return so that they can look
;;; past them. We assume CHAR is not whitespace.
(defun read-maybe-nothing (stream char)
  (let ((retval (multiple-value-list
		 (funcall (get-coerced-cmt-entry char *readtable*)
			  stream
			  char))))
    (if retval (rplacd retval nil))))

(defun read (&optional (stream *standard-input*)
		       (eof-error-p t)
		       (eof-value ())
		       (recursivep ()))
  #!+sb-doc
  "Read the next Lisp value from STREAM, and return it."
  (let ((result (read-preserving-whitespace stream
					    eof-error-p
					    eof-value
					    recursivep)))
    ;; This function generally discards trailing whitespace. If you
    ;; don't want to discard trailing whitespace, call
    ;; CL:READ-PRESERVING-WHITESPACE instead.
    (unless (or (eql result eof-value) recursivep)
      (let ((next-char (read-char stream nil nil)))
	(unless (or (null next-char)
		    (whitespacep next-char))
	  (unread-char next-char stream))))
    result))

;;; (This is a COMMON-LISP exported symbol.)
(defun read-delimited-list (endchar &optional
				    (input-stream *standard-input*)
				    recursive-p)
  #!+sb-doc
  "Read Lisp values from INPUT-STREAM until the next character after a
   value's representation is ENDCHAR, and return the objects as a list."
  (declare (ignore recursive-p))
  (do ((char (flush-whitespace input-stream)
	     (flush-whitespace input-stream))
       (retlist ()))
      ((char= char endchar) (unless *read-suppress* (nreverse retlist)))
    (setq retlist (nconc (read-maybe-nothing input-stream char) retlist))))

;;;; basic readmacro definitions
;;;;
;;;; Some large, hairy subsets of readmacro definitions (backquotes
;;;; and sharp macros) are not here, but in their own source files.

(defun read-quote (stream ignore)
  (declare (ignore ignore))
  (list 'quote (read stream t nil t)))

(defun read-comment (stream ignore)
  (declare (ignore ignore))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
	(prepare-for-fast-read-char stream
	  (do ((char (fast-read-char nil nil)
		     (fast-read-char nil nil)))
	      ((or (not char) (char= char #\newline))
	       (done-with-fast-read-char))))
	;; CLOS stream
	(do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
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
    (if (ansi-stream-p stream)
	(prepare-for-fast-read-char stream
	  (do ((char (fast-read-char t) (fast-read-char t)))
	      ((char= char closech)
	       (done-with-fast-read-char))
	    (if (escapep char) (setq char (fast-read-char t)))
	    (ouch-read-buffer char)))
	;; CLOS stream
	(do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
	    ((or (eq char :eof) (char= char closech))
	     (if (eq char :eof)
		 (error 'end-of-file :stream stream)))
	  (when (escapep char)
	    (setq char (read-char stream nil :eof))
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
;;;
;;; FIXME: why aren't these ATT-getting forms using GET-CAT-ENTRY?
;;; Because we've cached the readtable tables?
(defmacro char-class (char attarray atthash)
  `(let ((att (if (typep ,char 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
     (declare (fixnum att))
     (if (<= att +char-attr-terminating-macro+)
	 +char-attr-delimiter+
	 att)))

;;; Return the character class for CHAR, which might be part of a
;;; rational number.
(defmacro char-class2 (char attarray atthash)
  `(let ((att (if (typep ,char 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
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
(defmacro char-class3 (char attarray atthash)
  `(let ((att (if (typep ,char 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
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
		 (if (= att +char-attr-constituent-expt+)
		     +char-attr-constituent-digit-or-expt+
		     +char-attr-constituent-digit+)
		 +char-attr-constituent-decimal-digit+)
	     att))))

;;;; token fetching

(defvar *read-suppress* nil
  #!+sb-doc
  "Suppress most interpreting in the reader when T.")

(defvar *read-base* 10
  #!+sb-doc
  "the radix that Lisp reads numbers in")
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
  (let ((attribute-array (character-attribute-array *readtable*))
        (attribute-hash-table (character-attribute-hash-table *readtable*))
	(package-designator nil)
	(colons 0)
	(possibly-rational t)
	(seen-digit-or-expt nil)
	(possibly-float t)
	(was-possibly-float nil)
	(escapes ())
	(seen-multiple-escapes nil))
    (reset-read-buffer)
    (prog ((char firstchar))
      (case (char-class3 char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-sign+ (go SIGN))
	(#.+char-attr-constituent-digit+ (go LEFTDIGIT))
	(#.+char-attr-constituent-digit-or-expt+
	 (setq seen-digit-or-expt t)
	 (go LEFTDIGIT))
	(#.+char-attr-constituent-decimal-digit+ (go LEFTDECIMALDIGIT))
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
      (case (char-class3 char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go LEFTDIGIT))
	(#.+char-attr-constituent-digit-or-expt+
	 (setq seen-digit-or-expt t)
	 (go LEFTDIGIT))
	(#.+char-attr-constituent-decimal-digit+ (go LEFTDECIMALDIGIT))
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
      (setq was-possibly-float possibly-float)
      (case (char-class3 char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go LEFTDIGIT))
	(#.+char-attr-constituent-decimal-digit+ (if possibly-float
						     (go LEFTDECIMALDIGIT)
						     (go SYMBOL)))
	(#.+char-attr-constituent-dot+ (if possibly-float
					   (go MIDDLEDOT)
					   (go SYMBOL)))
	(#.+char-attr-constituent-digit-or-expt+
	 (if (or seen-digit-or-expt (not was-possibly-float))
	     (progn (setq seen-digit-or-expt t) (go LEFTDIGIT))
	     (progn (setq seen-digit-or-expt t) (go LEFTDIGIT-OR-EXPT))))
	(#.+char-attr-constituent-expt+
	 (if was-possibly-float
	     (go EXPONENT)
	     (go SYMBOL)))
	(#.+char-attr-constituent-slash+ (if possibly-rational
					     (go RATIO)
					     (go SYMBOL)))
	(#.+char-attr-delimiter+ (unread-char char stream)
				 (return (make-integer)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     LEFTDIGIT-OR-EXPT
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-integer)))
      (case (char-class3 char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go LEFTDIGIT))
	(#.+char-attr-constituent-decimal-digit+ (bug "impossible!"))
	(#.+char-attr-constituent-dot+ (go SYMBOL))
	(#.+char-attr-constituent-digit-or-expt+ (go LEFTDIGIT))
	(#.+char-attr-constituent-expt+ (go SYMBOL))
	(#.+char-attr-constituent-sign+ (go EXPTSIGN))
	(#.+char-attr-constituent-slash+ (if possibly-rational
					     (go RATIO)
					     (go SYMBOL)))
	(#.+char-attr-delimiter+ (unread-char char stream)
				 (return (make-integer)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     LEFTDECIMALDIGIT ; saw "[sign] {decimal-digit}+"
      (aver possibly-float)
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go LEFTDECIMALDIGIT))
	(#.+char-attr-constituent-dot+ (go MIDDLEDOT))
	(#.+char-attr-constituent-expt+ (go EXPONENT))
	(#.+char-attr-constituent-slash+ (aver (not possibly-rational))
					 (go SYMBOL))
	(#.+char-attr-delimiter+ (unread-char char stream)
				 (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     MIDDLEDOT ; saw "[sign] {digit}+ dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (let ((*read-base* 10))
			     (make-integer))))
      (case (char-class char attribute-array attribute-hash-table)
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
     RIGHTDIGIT ; saw "[sign] {decimal-digit}* dot {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float stream)))
      (case (char-class char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
	(#.+char-attr-constituent-expt+ (go EXPONENT))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (return (make-float stream)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     SIGNDOT ; saw "[sign] dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(t (go SYMBOL)))
     FRONTDOT ; saw "dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "dot context error"))
      (case (char-class char attribute-array attribute-hash-table)
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
      (setq possibly-float t)
      (case (char-class char attribute-array attribute-hash-table)
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
      (case (char-class char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go EXPTDIGIT))
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     EXPTDIGIT ; got to EXPONENT, saw "[sign] {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-float stream)))
      (case (char-class char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go EXPTDIGIT))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (return (make-float stream)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     RATIO ; saw "[sign] {digit}+ slash"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class2 char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go RATIODIGIT))
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     RATIODIGIT ; saw "[sign] {digit}+ slash {digit}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (return (make-ratio stream)))
      (case (char-class2 char attribute-array attribute-hash-table)
	(#.+char-attr-constituent-digit+ (go RATIODIGIT))
	(#.+char-attr-delimiter+
	 (unread-char char stream)
	 (return (make-ratio stream)))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
     DOTS ; saw "dot {dot}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (%reader-error stream "too many dots"))
      (case (char-class char attribute-array attribute-hash-table)
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
	(if (ansi-stream-p stream)
	    (prepare-for-fast-read-char stream
	      (prog ()
	       SYMBOL-LOOP
	       (ouch-read-buffer char)
	       (setq char (fast-read-char nil nil))
	       (unless char (go RETURN-SYMBOL))
	       (case (char-class char attribute-array attribute-hash-table)
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
	    ;; CLOS stream
	    (prog ()
	     SYMBOL-LOOP
	     (ouch-read-buffer char)
	     (setq char (read-char stream nil :eof))
	     (when (eq char :eof) (go RETURN-SYMBOL))
	     (case (char-class char attribute-array attribute-hash-table)
	       (#.+char-attr-escape+ (go ESCAPE))
	       (#.+char-attr-delimiter+ (unread-char char stream)
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
      (case (char-class char attribute-array attribute-hash-table)
	(#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
	(#.+char-attr-escape+ (go ESCAPE))
	(#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
	(#.+char-attr-package-delimiter+ (go COLON))
	(t (go SYMBOL)))
      MULT-ESCAPE
      (setq seen-multiple-escapes t)
      (do ((char (read-char stream t) (read-char stream t)))
	  ((multiple-escape-p char))
	(if (escapep char) (setq char (read-char stream t)))
	(push *ouch-ptr* escapes)
	(ouch-read-buffer char))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
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
		(if seen-multiple-escapes
		    (read-buffer-to-string)
		    *keyword-package*)))
      (reset-read-buffer)
      (setq escapes ())
      (setq char (read-char stream nil nil))
      (unless char (reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-array attribute-hash-table)
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
      (case (char-class char attribute-array attribute-hash-table)
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

(defun make-float (stream)
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
				      *read-default-float-format*
				      stream)))
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
	   ;; Generate and return the float, depending on FLOAT-CHAR:
	   (let* ((float-format (case (char-upcase float-char)
				  (#\E *read-default-float-format*)
				  (#\S 'short-float)
				  (#\F 'single-float)
				  (#\D 'double-float)
				  (#\L 'long-float)))
		  (result (make-float-aux (* (expt 10 exponent) number)
					  divisor float-format stream)))
	     (return-from make-float
	       (if negative-fraction (- result) result))))
	  (t (bug "bad fallthrough in floating point reader")))))

(defun make-float-aux (number divisor float-format stream)
  (handler-case
      (coerce (/ number divisor) float-format)
    (type-error (c)
      (error 'reader-impossible-number-error
	     :error c :stream stream
	     :format-control "failed to build float"))))

(defun make-ratio (stream)
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
    (let ((num (handler-case
		   (/ numerator denominator)
		 (arithmetic-error (c)
		   (error 'reader-impossible-number-error
			  :error c :stream stream
			  :format-control "failed to build ratio")))))
      (if negative-number (- num) num))))

;;;; cruft for dispatch macros

(defun make-char-dispatch-table ()
  (make-hash-table))

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
         (rt (or rt *standard-readtable*))
	 (dpair (find disp-char (dispatch-tables rt)
		      :test #'char= :key #'car)))
    (if dpair
	(setf (gethash sub-char (cdr dpair)) (coerce function 'function))
	(error "~S is not a dispatch char." disp-char))))

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (rt *readtable*))
  #!+sb-doc
  "Return the macro character function for SUB-CHAR under DISP-CHAR
   or NIL if there is no associated function."
  (let* ((sub-char (char-upcase sub-char))
         (rt (or rt *standard-readtable*))
         (dpair (find disp-char (dispatch-tables rt)
                      :test #'char= :key #'car)))
    (if dpair
        (values (gethash sub-char (cdr dpair)))
        (error "~S is not a dispatch char." disp-char))))

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
                     (gethash sub-char (cdr dpair) #'dispatch-char-error))
		   stream sub-char (if numargp numarg nil))
	  (%reader-error stream "no dispatch table for dispatch char")))))

;;;; READ-FROM-STRING

;;; FIXME: Is it really worth keeping this pool?
(defvar *read-from-string-spares* ()
  #!+sb-doc
  "A resource of string streams for Read-From-String.")

(defun read-from-string (string &optional (eof-error-p t) eof-value
				&key (start 0) end
				preserve-whitespace)
  #!+sb-doc
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (declare (string string))
  
  (with-array-data ((string string)
		    (start start)
		    (end (%check-vector-sequence-bounds string start end)))
    (unless *read-from-string-spares*
      (push (make-string-input-stream "" 0 0) *read-from-string-spares*))
    (let ((stream (pop *read-from-string-spares*)))
      (setf (string-input-stream-string stream)
	    (coerce string '(simple-array character (*))))
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
  (macrolet ((parse-error (format-control)
	       `(error 'simple-parse-error
		       :format-control ,format-control
		       :format-arguments (list string))))
    (with-array-data ((string string :offset-var offset)
		      (start start)
		      (end (%check-vector-sequence-bounds string start end)))
      (let ((index (do ((i start (1+ i)))
		       ((= i end)
			(if junk-allowed
			    (return-from parse-integer (values nil end))
			    (parse-error "no non-whitespace characters in string ~S.")))
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
                  (loop
                   (incf index)
                   (when (= index end) (return))
                   (unless (whitespacep (char string index))
		      (parse-error "junk in string ~S")))
		  (return nil))
		 (t
		  (parse-error "junk in string ~S"))))
	 (incf index))
	(values
	 (if found-digit
	     (if minusp (- result) result)
	     (if junk-allowed
		 nil
		 (parse-error "no digits in string ~S")))
	 (- index offset))))))

;;;; reader initialization code

(defun !reader-cold-init ()
  (!cold-init-read-buffer)
  (!cold-init-secondary-attribute-table)
  (!cold-init-standard-readtable)
  ;; FIXME: This was commented out, but should probably be restored.
  #+nil (!cold-init-integer-reader))

(def!method print-object ((readtable readtable) stream)
  (print-unreadable-object (readtable stream :identity t :type t)))
