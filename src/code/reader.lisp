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

;;; A standard Lisp readtable (once cold-init is through). This is for
;;; recovery from broken read-tables (and for
;;; WITH-STANDARD-IO-SYNTAX), and should not normally be user-visible.
(defvar *standard-readtable* nil)

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

;;; If The Gods didn't intend for us to use multiple namespaces, why
;;; did They specify them?
(defun simple-reader-error (stream control &rest args)
  (error 'simple-reader-error
         :stream stream
         :format-control control
         :format-arguments args))

;;;; macros and functions for character tables

(defun get-cat-entry (char rt)
  (declare (readtable rt))
  (if (typep char 'base-char)
      (elt (character-attribute-array rt) (char-code char))
      (values (gethash char (character-attribute-hash-table rt)
                       +char-attr-constituent+))))

(defun set-cat-entry (char newvalue &optional (rt *readtable*))
  (declare (readtable rt))
  (if (typep char 'base-char)
      (setf (elt (character-attribute-array rt) (char-code char)) newvalue)
      (if (= newvalue +char-attr-constituent+)
          ;; Default value for the C-A-HASH-TABLE is +CHAR-ATTR-CONSTITUENT+.
          (%remhash char (character-attribute-hash-table rt))
          (setf (gethash char (character-attribute-hash-table rt)) newvalue)))
  (values))

;;; the value actually stored in the character macro table. As per
;;; ANSI #'GET-MACRO-CHARACTER and #'SET-MACRO-CHARACTER, this can
;;; be either a function or NIL.
(defun get-raw-cmt-entry (char readtable)
  (declare (readtable readtable))
  (if (typep char 'base-char)
      (svref (character-macro-array readtable) (char-code char))
      ;; Note: DEFAULT here is NIL, not #'UNDEFINED-MACRO-CHAR, so
      ;; that everything above the base-char range is a non-macro
      ;; constituent by default.
      (values (gethash char (character-macro-hash-table readtable) nil))))

;;; the value represented by whatever is stored in the character macro
;;; table. As per ANSI #'GET-MACRO-CHARACTER and #'SET-MACRO-CHARACTER,
;;; a function value represents itself, and a NIL value represents the
;;; default behavior.
(defun get-coerced-cmt-entry (char readtable)
  (the function
    (or (get-raw-cmt-entry char readtable)
        #'read-token)))

(defun set-cmt-entry (char new-value-designator &optional (rt *readtable*))
  (let ((new (when new-value-designator
               (%coerce-callable-to-fun new-value-designator))))
    (if (typep char 'base-char)
        (setf (svref (character-macro-array rt) (char-code char)) new)
        (setf (gethash char (character-macro-hash-table rt)) new))))

(defun undefined-macro-char (stream char)
  (unless *read-suppress*
    (simple-reader-error stream "undefined read-macro character ~S" char)))

;;; The character attribute table is a CHAR-CODE-LIMIT vector of integers.

(defmacro test-attribute (char whichclass rt)
  `(= (the fixnum (get-cat-entry ,char ,rt)) ,whichclass))

;;; predicates for testing character attributes

#!-sb-fluid
(progn
  (declaim (inline whitespace[1]p whitespace[2]p))
  (declaim (inline constituentp terminating-macrop))
  (declaim (inline single-escape-p multiple-escape-p))
  (declaim (inline token-delimiterp)))

;;; the [1] and [2] here refer to ANSI glossary entries for
;;; "whitespace".
(defun whitespace[1]p (char)
  (test-attribute char +char-attr-whitespace+ *standard-readtable*))
(defun whitespace[2]p (char &optional (rt *readtable*))
  (test-attribute char +char-attr-whitespace+ rt))

(defun constituentp (char &optional (rt *readtable*))
  (test-attribute char +char-attr-constituent+ rt))

(defun terminating-macrop (char &optional (rt *readtable*))
  (test-attribute char +char-attr-terminating-macro+ rt))

(defun single-escape-p (char &optional (rt *readtable*))
  (test-attribute char +char-attr-single-escape+ rt))

(defun multiple-escape-p (char &optional (rt *readtable*))
  (test-attribute char +char-attr-multiple-escape+ rt))

(defun token-delimiterp (char &optional (rt *readtable*))
  ;; depends on actual attribute numbering in readtable.lisp.
  (<= (get-cat-entry char rt) +char-attr-terminating-macro+))

;;;; constituent traits (see ANSI 2.1.4.2)

;;; There are a number of "secondary" attributes which are constant
;;; properties of characters (as long as they are constituents).

(defvar *constituent-trait-table*)
(declaim (type attribute-table *constituent-trait-table*))

(defun !set-constituent-trait (char trait)
  (aver (typep char 'base-char))
  (setf (elt *constituent-trait-table* (char-code char))
        trait))

(defun !cold-init-constituent-trait-table ()
  (setq *constituent-trait-table*
        (make-array base-char-code-limit :element-type '(unsigned-byte 8)
                    :initial-element +char-attr-constituent+))
  (!set-constituent-trait #\: +char-attr-package-delimiter+)
  (!set-constituent-trait #\. +char-attr-constituent-dot+)
  (!set-constituent-trait #\+ +char-attr-constituent-sign+)
  (!set-constituent-trait #\- +char-attr-constituent-sign+)
  (!set-constituent-trait #\/ +char-attr-constituent-slash+)
  (do ((i (char-code #\0) (1+ i)))
      ((> i (char-code #\9)))
    (!set-constituent-trait (code-char i) +char-attr-constituent-digit+))
  (!set-constituent-trait #\E +char-attr-constituent-expt+)
  (!set-constituent-trait #\F +char-attr-constituent-expt+)
  (!set-constituent-trait #\D +char-attr-constituent-expt+)
  (!set-constituent-trait #\S +char-attr-constituent-expt+)
  (!set-constituent-trait #\L +char-attr-constituent-expt+)
  (!set-constituent-trait #\e +char-attr-constituent-expt+)
  (!set-constituent-trait #\f +char-attr-constituent-expt+)
  (!set-constituent-trait #\d +char-attr-constituent-expt+)
  (!set-constituent-trait #\s +char-attr-constituent-expt+)
  (!set-constituent-trait #\l +char-attr-constituent-expt+)
  (!set-constituent-trait #\Space +char-attr-invalid+)
  (!set-constituent-trait #\Newline +char-attr-invalid+)
  (dolist (c (list backspace-char-code tab-char-code form-feed-char-code
                   return-char-code rubout-char-code))
    (!set-constituent-trait (code-char c) +char-attr-invalid+)))

(declaim (inline get-constituent-trait))
(defun get-constituent-trait (char)
  (if (typep char 'base-char)
      (elt *constituent-trait-table* (char-code char))
      +char-attr-constituent+))

;;;; Readtable Operations

(defun assert-not-standard-readtable (readtable operation)
  (when (eq readtable *standard-readtable*)
    (cerror "Frob it anyway!" 'standard-readtable-modified-error
            :operation operation)))

(defun readtable-case (readtable)
  (%readtable-case readtable))

(defun (setf readtable-case) (case readtable)
  (assert-not-standard-readtable readtable '(setf readtable-case))
  (setf (%readtable-case readtable) case))

(defun shallow-replace/eql-hash-table (to from)
  (maphash (lambda (k v) (setf (gethash k to) v)) from))

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (assert-not-standard-readtable to-readtable 'copy-readtable)
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
                             (to-readtable *readtable*) (from-readtable nil))
  #!+sb-doc
  "Causes the syntax of TO-CHAR to be the same as FROM-CHAR in the optional
readtable (defaults to the current readtable). The FROM-TABLE defaults to the
standard Lisp readtable when NIL."
  (assert-not-standard-readtable to-readtable 'set-syntax-from-char)
  (let ((really-from-readtable (or from-readtable *standard-readtable*)))
    (let ((att (get-cat-entry from-char really-from-readtable))
          (mac (get-raw-cmt-entry from-char really-from-readtable))
          (from-dpair (find from-char (dispatch-tables really-from-readtable)
                            :test #'char= :key #'car))
          (to-dpair (find to-char (dispatch-tables to-readtable)
                          :test #'char= :key #'car)))
      (set-cat-entry to-char att to-readtable)
      (set-cmt-entry to-char mac to-readtable)
      (when from-dpair
        (cond
          (to-dpair
           (let ((table (cdr to-dpair)))
             (clrhash table)
             (shallow-replace/eql-hash-table table (cdr from-dpair))))
          (t
           (let ((pair (cons to-char (make-hash-table))))
             (shallow-replace/eql-hash-table (cdr pair) (cdr from-dpair))
             (setf (dispatch-tables to-readtable)
                   (push pair (dispatch-tables to-readtable)))))))))
  t)

(defun set-macro-character (char function &optional
                                 (non-terminatingp nil)
                                 (rt-designator *readtable*))
  #!+sb-doc
  "Causes CHAR to be a macro character which invokes FUNCTION when seen
   by the reader. The NON-TERMINATINGP flag can be used to make the macro
   character non-terminating, i.e. embeddable in a symbol name."
  (let ((designated-readtable (or rt-designator *standard-readtable*)))
    (assert-not-standard-readtable designated-readtable 'set-macro-character)
    (set-cat-entry char (if non-terminatingp
                            +char-attr-constituent+
                            +char-attr-terminating-macro+)
                   designated-readtable)
    (set-cmt-entry char function designated-readtable)
    t)) ; (ANSI-specified return value)

(defun get-macro-character (char &optional (rt-designator *readtable*))
  #!+sb-doc
  "Return the function associated with the specified CHAR which is a macro
  character, or NIL if there is no such function. As a second value, return
  T if CHAR is a macro character which is non-terminating, i.e. which can
  be embedded in a symbol name."
  (let* ((designated-readtable (or rt-designator *standard-readtable*))
         ;; the first return value: a FUNCTION if CHAR is a macro
         ;; character, or NIL otherwise
         (fun-value (get-raw-cmt-entry char designated-readtable)))
    (values fun-value
            ;; NON-TERMINATING-P return value:
            (if fun-value
                (or (constituentp char designated-readtable)
                    (not (terminating-macrop char designated-readtable)))
                ;; ANSI's definition of GET-MACRO-CHARACTER says this
                ;; value is NIL when CHAR is not a macro character.
                ;; I.e. this value means not just "non-terminating
                ;; character?" but "non-terminating macro character?".
                nil))))


(defun make-char-dispatch-table ()
  (make-hash-table))

(defun make-dispatch-macro-character (char &optional
                                      (non-terminating-p nil)
                                      (rt *readtable*))
  #!+sb-doc
  "Cause CHAR to become a dispatching macro character in readtable (which
   defaults to the current readtable). If NON-TERMINATING-P, the char will
   be non-terminating."
  ;; Checks already for standard readtable modification.
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
                                     &optional (rt-designator *readtable*))
  #!+sb-doc
  "Cause FUNCTION to be called whenever the reader reads DISP-CHAR
   followed by SUB-CHAR."
  ;; Get the dispatch char for macro (error if not there), diddle
  ;; entry for sub-char.
  (let* ((sub-char (char-upcase sub-char))
         (readtable (or rt-designator *standard-readtable*)))
    (assert-not-standard-readtable readtable 'set-dispatch-macro-character)
    (when (digit-char-p sub-char)
      (error "SUB-CHAR must not be a decimal digit: ~S" sub-char))
    (let ((dpair (find disp-char (dispatch-tables readtable)
                       :test #'char= :key #'car)))
      (if dpair
          (setf (gethash sub-char (cdr dpair)) (coerce function 'function))
          (error "~S is not a dispatch char." disp-char))))
  t)

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (rt-designator *readtable*))
  #!+sb-doc
  "Return the macro character function for SUB-CHAR under DISP-CHAR
   or NIL if there is no associated function."
  (let* ((sub-char  (char-upcase sub-char))
         (readtable (or rt-designator *standard-readtable*))
         (dpair     (find disp-char (dispatch-tables readtable)
                          :test #'char= :key #'car)))
    (if dpair
        (values (gethash sub-char (cdr dpair)))
        (error "~S is not a dispatch char." disp-char))))


;;;; definitions to support internal programming conventions

(declaim (inline eofp))
(defun eofp (char)
  (eq char *eof-object*))

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
                         (gethash char attribute-hash-table
                                  +char-attr-constituent+)))
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
                           (gethash char attribute-hash-table
                                    +char-attr-constituent+)))
                     +char-attr-whitespace+))
             (if (eq char :eof)
                 (error 'end-of-file :stream stream)
                 char))))))

;;;; temporary initialization hack

;; Install the (easy) standard macro-chars into *READTABLE*.
(defun !cold-init-standard-readtable ()
  (/show0 "entering !cold-init-standard-readtable")
  ;; All characters get boring defaults in MAKE-READTABLE. Now we
  ;; override the boring defaults on characters which need more
  ;; interesting behavior.
  (flet ((whitespaceify (char)
           (set-cmt-entry char nil)
           (set-cat-entry char +char-attr-whitespace+)))
    (whitespaceify (code-char tab-char-code))
    (whitespaceify #\Newline)
    (whitespaceify #\Space)
    (whitespaceify (code-char form-feed-char-code))
    (whitespaceify (code-char return-char-code)))

  (set-cat-entry #\\ +char-attr-single-escape+)
  (set-cmt-entry #\\ nil)

  (set-cat-entry #\| +char-attr-multiple-escape+)
  (set-cmt-entry #\| nil)

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
    (when (constituentp char)
      (set-cmt-entry char nil)))

  (/show0 "leaving !cold-init-standard-readtable"))

;;;; implementation of the read buffer

(defvar *read-buffer*)

(defvar *inch-ptr*) ; *OUCH-PTR* always points to next char to write.
(defvar *ouch-ptr*) ; *INCH-PTR* always points to next char to read.

(declaim (type index *inch-ptr* *ouch-ptr*))
(declaim (type (simple-array character (*)) *read-buffer*))

(declaim (inline reset-read-buffer))
(defun reset-read-buffer ()
  ;; Turn *READ-BUFFER* into an empty read buffer.
  (setq *ouch-ptr* 0)
  (setq *inch-ptr* 0))

(declaim (inline ouch-read-buffer))
(defun ouch-read-buffer (char)
  ;; When buffer overflow
  (let ((op *ouch-ptr*))
    (declare (optimize (sb!c::insert-array-bounds-checks 0)))
    (when (>= op (length *read-buffer*))
    ;; Size should be doubled.
      (grow-read-buffer))
    (setf (elt *read-buffer* op) char)
    (setq *ouch-ptr* (1+ op))))

(defun grow-read-buffer ()
  (let* ((rbl (length *read-buffer*))
         (new-length (* 2 rbl))
         (new-buffer (make-string new-length)))
    (setq *read-buffer* (replace new-buffer *read-buffer*))))

(defun inch-read-buffer ()
  (if (>= *inch-ptr* *ouch-ptr*)
      *eof-object*
      (prog1
          (elt *read-buffer* *inch-ptr*)
        (incf *inch-ptr*))))

(declaim (inline unread-buffer))
(defun unread-buffer ()
  (decf *inch-ptr*))

(declaim (inline read-unwind-read-buffer))
(defun read-unwind-read-buffer ()
  ;; Keep contents, but make next (INCH..) return first character.
  (setq *inch-ptr* 0))

(defun read-buffer-to-string ()
  (subseq *read-buffer* 0 *ouch-ptr*))

(defmacro with-read-buffer (() &body body)
  `(let* ((*read-buffer* (make-string 128))
          (*ouch-ptr* 0)
          (*inch-ptr* 0))
     ,@body))

(declaim (inline read-buffer-boundp))
(defun read-buffer-boundp ()
  (and (boundp '*read-buffer*)
       (boundp '*ouch-ptr*)
       (boundp '*inch-ptr*)))

(defun check-for-recursive-read (stream recursive-p operator-name)
  (when (and recursive-p (not (read-buffer-boundp)))
    (simple-reader-error
     stream
     "~A was invoked with RECURSIVE-P being true outside ~
      of a recursive read operation."
     `(,operator-name))))

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

;;; Like READ-PRESERVING-WHITESPACE, but doesn't check the read buffer
;;; for being set up properly.
(defun %read-preserving-whitespace (stream eof-error-p eof-value recursive-p)
  (if recursive-p
      ;; a loop for repeating when a macro returns nothing
      (loop
       (let ((char (read-char stream eof-error-p *eof-object*)))
         (cond ((eofp char) (return eof-value))
               ((whitespace[2]p char))
               (t
                (let* ((macrofun (get-coerced-cmt-entry char *readtable*))
                       (result (multiple-value-list
                                (funcall macrofun stream char))))
                  ;; Repeat if macro returned nothing.
                  (when result
                    (return (unless *read-suppress* (car result)))))))))
      (let ((*sharp-equal-alist* nil))
        (with-read-buffer ()
          (%read-preserving-whitespace stream eof-error-p eof-value t)))))

;;; READ-PRESERVING-WHITESPACE behaves just like READ, only it makes
;;; sure to leave terminating whitespace in the stream. (This is a
;;; COMMON-LISP exported symbol.)
(defun read-preserving-whitespace (&optional (stream *standard-input*)
                                             (eof-error-p t)
                                             (eof-value nil)
                                             (recursive-p nil))
  #!+sb-doc
  "Read from STREAM and return the value read, preserving any whitespace
   that followed the object."
  (check-for-recursive-read stream recursive-p 'read-preserving-whitespace)
  (%read-preserving-whitespace stream eof-error-p eof-value recursive-p))

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
                       (eof-value nil)
                       (recursive-p nil))
  #!+sb-doc
  "Read the next Lisp value from STREAM, and return it."
  (check-for-recursive-read stream recursive-p 'read)
  (let ((result (%read-preserving-whitespace stream eof-error-p eof-value
                                             recursive-p)))
    ;; This function generally discards trailing whitespace. If you
    ;; don't want to discard trailing whitespace, call
    ;; CL:READ-PRESERVING-WHITESPACE instead.
    (unless (or (eql result eof-value) recursive-p)
      (let ((next-char (read-char stream nil nil)))
        (unless (or (null next-char)
                    (whitespace[2]p next-char))
          (unread-char next-char stream))))
    result))

;;; (This is a COMMON-LISP exported symbol.)
(defun read-delimited-list (endchar &optional
                                    (input-stream *standard-input*)
                                    recursive-p)
  #!+sb-doc
  "Read Lisp values from INPUT-STREAM until the next character after a
   value's representation is ENDCHAR, and return the objects as a list."
  (check-for-recursive-read input-stream recursive-p 'read-delimited-list)
  (flet ((%read-delimited-list (endchar input-stream)
           (do ((char (flush-whitespace input-stream)
                      (flush-whitespace input-stream))
                (retlist ()))
               ((char= char endchar)
                (unless *read-suppress* (nreverse retlist)))
             (setq retlist (nconc (read-maybe-nothing input-stream char)
                                  retlist)))))
    (declare (inline %read-delimited-list))
    (if recursive-p
        (%read-delimited-list endchar input-stream)
        (with-read-buffer ()
          (%read-delimited-list endchar input-stream)))))

;;;; basic readmacro definitions
;;;;
;;;; Some large, hairy subsets of readmacro definitions (backquotes
;;;; and sharp macros) are not here, but in their own source files.

(defun read-quote (stream ignore)
  (declare (ignore ignore))
  (list 'quote (read stream t nil t)))

(defun read-comment (stream ignore)
  (declare (ignore ignore))
  (handler-bind
      ((character-decoding-error
        #'(lambda (decoding-error)
            (declare (ignorable decoding-error))
            (style-warn
             'sb!kernel::character-decoding-error-in-macro-char-comment
             :position (file-position stream) :stream stream)
            (invoke-restart 'attempt-resync))))
    (let ((stream (in-synonym-of stream)))
      (if (ansi-stream-p stream)
          (prepare-for-fast-read-char stream
           (do ((char (fast-read-char nil nil)
                      (fast-read-char nil nil)))
               ((or (not char) (char= char #\newline))
                (done-with-fast-read-char))))
          ;; CLOS stream
          (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
              ((or (eq char :eof) (char= char #\newline)))))))
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
                            (unless *read-suppress*
                              (simple-reader-error
                               stream
                               "Nothing appears before . in list.")))
                           ((whitespace[2]p nextchar)
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
         (if *read-suppress*
             (return-from read-after-dot nil)
             (simple-reader-error stream "Nothing appears after . in list.")))
      ;; See whether there's something there.
      (setq lastobj (read-maybe-nothing stream char))
      (when lastobj (return t)))
    ;; At least one thing appears after the dot.
    ;; Check for more than one thing following dot.
    (do ((lastchar (flush-whitespace stream)
                   (flush-whitespace stream)))
        ((char= lastchar #\) ) lastobj) ;success!
      ;; Try reading virtual whitespace.
      (if (and (read-maybe-nothing stream lastchar)
               (not *read-suppress*))
          (simple-reader-error stream
                               "More than one object follows . in list.")))))

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
            (if (single-escape-p char) (setq char (fast-read-char t)))
            (ouch-read-buffer char)))
        ;; CLOS stream
        (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
            ((or (eq char :eof) (char= char closech))
             (if (eq char :eof)
                 (error 'end-of-file :stream stream)))
          (when (single-escape-p char)
            (setq char (read-char stream nil :eof))
            (if (eq char :eof)
                (error 'end-of-file :stream stream)))
          (ouch-read-buffer char))))
  (read-buffer-to-string))

(defun read-right-paren (stream ignore)
  (declare (ignore ignore))
  (simple-reader-error stream "unmatched close parenthesis"))

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
    (cond ((single-escape-p char)
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
                ((single-escape-p ch)
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
                      (eql (get-constituent-trait char)
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
     (cond
       ((<= att +char-attr-terminating-macro+) +char-attr-delimiter+)
       ((< att +char-attr-constituent+) att)
       (t (setf att (get-constituent-trait ,char))
          (if (= att +char-attr-invalid+)
              (simple-reader-error stream "invalid constituent")
              att)))))

;;; Return the character class for CHAR, which might be part of a
;;; rational number.
(defmacro char-class2 (char attarray atthash)
  `(let ((att (if (typep ,char 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
     (declare (fixnum att))
     (cond
       ((<= att +char-attr-terminating-macro+) +char-attr-delimiter+)
       ((< att +char-attr-constituent+) att)
       (t (setf att (get-constituent-trait ,char))
          (cond
            ((digit-char-p ,char *read-base*) +char-attr-constituent-digit+)
            ((= att +char-attr-constituent-digit+) +char-attr-constituent+)
            ((= att +char-attr-invalid+)
             (simple-reader-error stream "invalid constituent"))
            (t att))))))

;;; Return the character class for a char which might be part of a
;;; rational or floating number. (Assume that it is a digit if it
;;; could be.)
(defmacro char-class3 (char attarray atthash)
  `(let ((att (if (typep ,char 'base-char)
                  (aref ,attarray (char-code ,char))
                  (gethash ,char ,atthash +char-attr-constituent+))))
     (declare (fixnum att))
     (cond
       ((<= att +char-attr-terminating-macro+) +char-attr-delimiter+)
       ((< att +char-attr-constituent+) att)
       (t (setf att (get-constituent-trait ,char))
          (when possibly-rational
            (setq possibly-rational
                  (or (digit-char-p ,char *read-base*)
                      (= att +char-attr-constituent-slash+))))
          (when possibly-float
            (setq possibly-float
                  (or (digit-char-p ,char 10)
                      (= att +char-attr-constituent-dot+))))
          (cond
            ((digit-char-p ,char (max *read-base* 10))
             (if (digit-char-p ,char *read-base*)
                 (if (= att +char-attr-constituent-expt+)
                     +char-attr-constituent-digit-or-expt+
                     +char-attr-constituent-digit+)
                 +char-attr-constituent-decimal-digit+))
            ((= att +char-attr-invalid+)
             (simple-reader-error stream "invalid constituent"))
            (t att))))))

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
      ;; Pull the special variable access out of the loop.
      (let ((buffer *read-buffer*))
        (dotimes (i *ouch-ptr*)
          (declare (optimize (sb!c::insert-array-bounds-checks 0)))
          (setf (schar buffer i) (char-upcase (schar buffer i))))))
     ((eq case :preserve))
     (t
      (macrolet ((skip-esc (&body body)
                   `(do ((i (1- *ouch-ptr*) (1- i))
                         (buffer *read-buffer*)
                         (escapes escapes))
                        ((minusp i))
                      (declare (fixnum i)
                               (optimize (sb!c::insert-array-bounds-checks 0)))
                      (when (or (null escapes)
                                (let ((esc (first escapes)))
                                  (declare (fixnum esc))
                                  (cond ((< esc i) t)
                                        (t
                                         (aver (= esc i))
                                         (pop escapes)
                                         nil))))
                        (let ((ch (schar buffer i)))
                          ,@body)))))
        (flet ((lower-em ()
                 (skip-esc (setf (schar buffer i) (char-downcase ch))))
               (raise-em ()
                 (skip-esc (setf (schar buffer i) (char-upcase ch)))))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-invalid+ (simple-reader-error stream
                                                    "invalid constituent"))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (t (go SYMBOL)))
     FRONTDOT ; saw "dot"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (simple-reader-error stream "dot context error"))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-constituent-dot+ (go DOTS))
        (#.+char-attr-delimiter+  (simple-reader-error stream
                                                       "dot context error"))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     DOTS ; saw "dot {dot}+"
      (ouch-read-buffer char)
      (setq char (read-char stream nil nil))
      (unless char (simple-reader-error stream "too many dots"))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-dot+ (go DOTS))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (simple-reader-error stream "too many dots"))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
                 (#.+char-attr-single-escape+ (done-with-fast-read-char)
                                              (go SINGLE-ESCAPE))
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
               (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
               (#.+char-attr-delimiter+ (unread-char char stream)
                            (go RETURN-SYMBOL))
               (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
               (#.+char-attr-package-delimiter+ (go COLON))
               (t (go SYMBOL-LOOP))))))
     SINGLE-ESCAPE ; saw a single-escape
      ;; Don't put the escape character in the read buffer.
      ;; READ-NEXT CHAR, put in buffer (no case conversion).
      (let ((nextchar (read-char stream nil nil)))
        (unless nextchar
          (reader-eof-error stream "after single-escape character"))
        (push *ouch-ptr* escapes)
        (ouch-read-buffer nextchar))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      MULT-ESCAPE
      (setq seen-multiple-escapes t)
      (do ((char (read-char stream t) (read-char stream t)))
          ((multiple-escape-p char))
        (if (single-escape-p char) (setq char (read-char stream t)))
        (push *ouch-ptr* escapes)
        (ouch-read-buffer char))
      (setq char (read-char stream nil nil))
      (unless char (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      COLON
      (casify-read-buffer escapes)
      (unless (zerop colons)
        (simple-reader-error stream
                             "too many colons in ~S"
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
         (simple-reader-error stream
                              "illegal terminating character after a colon: ~S"
                              char))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
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
         (simple-reader-error stream
                              "illegal terminating character after a colon: ~S"
                              char))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+
         (simple-reader-error stream
                              "too many colons after ~S name"
                              package-designator))
        (t (go SYMBOL)))
      RETURN-SYMBOL
      (casify-read-buffer escapes)
      (let ((found (if package-designator
                       (find-package package-designator)
                       (sane-package))))
        (unless found
          (error 'simple-reader-package-error :stream stream
                 :format-arguments (list package-designator)
                 :format-control "package ~S not found"))

        (if (or (zerop colons) (= colons 2) (eq found *keyword-package*))
            (return (intern* *read-buffer* *ouch-ptr* found))
            (multiple-value-bind (symbol test)
                (find-symbol* *read-buffer* *ouch-ptr* found)
              (when (eq test :external) (return symbol))
              (let ((name (read-buffer-to-string)))
                (with-simple-restart (continue "Use symbol anyway.")
                  (error 'simple-reader-package-error :stream stream
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

;;;; General reader for dispatch macros

(defun dispatch-char-error (stream sub-char ignore)
  (declare (ignore ignore))
  (if *read-suppress*
      (values)
      (simple-reader-error stream
                           "no dispatch function defined for ~S"
                           sub-char)))

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
          (simple-reader-error stream
                               "no dispatch table for dispatch char")))))

;;;; READ-FROM-STRING

(defun maybe-note-read-from-string-signature-issue (eof-error-p)
  ;; The interface is so unintuitive that we explicitly check for the common
  ;; error.
  (when (member eof-error-p '(:start :end :preserve-whitespace))
    (style-warn "~@<~S as EOF-ERROR-P argument to ~S: probable error. ~
               Two optional arguments must be provided before the ~
               first keyword argument.~:@>"
                eof-error-p 'read-from-string)
    t))

(declaim (ftype (sfunction (string t t index (or null index) t) (values t index))
                %read-from-string))
(defun %read-from-string (string eof-error-p eof-value start end preserve-whitespace)
  (with-array-data ((string string :offset-var offset)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (let ((stream (make-string-input-stream string start end)))
      (values (if preserve-whitespace
                  (%read-preserving-whitespace stream eof-error-p eof-value nil)
                  (read stream eof-error-p eof-value))
              (- (string-input-stream-current stream) offset)))))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace)
  #!+sb-doc
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (declare (string string))
  (maybe-note-read-from-string-signature-issue eof-error-p)
  (%read-from-string string eof-error-p eof-value start end preserve-whitespace))

(define-compiler-macro read-from-string (&whole form string &rest args)
  ;; Check this at compile-time, and rewrite it so we're silent at runtime.
  (destructuring-bind (&optional (eof-error-p t) eof-value &rest keys)
      args
    (cond ((maybe-note-read-from-string-signature-issue eof-error-p)
           `(read-from-string ,string t ,eof-value ,@keys))
          (t
           (let* ((start (gensym "START"))
                  (end (gensym "END"))
                  (preserve-whitespace (gensym "PRESERVE-WHITESPACE"))
                  bind seen ignore)
             (do ()
                 ((not (cdr keys))
                  ;; Odd number of keys, punt.
                  (when keys (return-from read-from-string form)))
               (let* ((key (pop keys))
                      (value (pop keys))
                      (var (case key
                             (:start start)
                             (:end end)
                             (:preserve-whitespace preserve-whitespace)
                             (otherwise
                              (return-from read-from-string form)))))
                 (when (member key seen)
                   (setf var (gensym "IGNORE"))
                   (push var ignore))
                 (push key seen)
                 (push (list var value) bind)))
             (dolist (default (list (list start 0)
                                    (list end nil)
                                    (list preserve-whitespace nil)))
               (unless (assoc (car default) bind)
                 (push default bind)))
             (once-only ((string string))
               `(let ,(nreverse bind)
                  ,@(when ignore `((declare (ignore ,@ignore))))
                  (%read-from-string ,string ,eof-error-p ,eof-value
                                     ,start ,end ,preserve-whitespace))))))))

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
                      (end end)
                      :check-fill-pointer t)
      (let ((index (do ((i start (1+ i)))
                       ((= i end)
                        (if junk-allowed
                            (return-from parse-integer (values nil end))
                            (parse-error "no non-whitespace characters in string ~S.")))
                     (declare (fixnum i))
                     (unless (whitespace[1]p (char string i)) (return i))))
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
                 ((whitespace[1]p char)
                  (loop
                   (incf index)
                   (when (= index end) (return))
                   (unless (whitespace[1]p (char string index))
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
  (!cold-init-constituent-trait-table)
  (!cold-init-standard-readtable)
  ;; FIXME: This was commented out, but should probably be restored.
  #+nil (!cold-init-integer-reader))

(def!method print-object ((readtable readtable) stream)
  (print-unreadable-object (readtable stream :identity t :type t)))
