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
(!defvar *read-default-float-format* 'single-float)
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
;;; If the initial value is changed from NIL to something more interesting,
;;; be sure to update the duplicated definition in "src/code/print.lisp"
(defglobal *standard-readtable* nil)

;;; In case we get an error trying to parse a symbol, we want to rebind the
;;; above stuff so it's cool.


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

(declaim (ftype (sfunction (character readtable) (unsigned-byte 8))
                get-cat-entry))
(defun get-cat-entry (char rt)
  (if (typep char 'base-char)
      (elt (character-attribute-array rt) (char-code char))
      (values (gethash char (character-attribute-hash-table rt)
                       +char-attr-constituent+))))

(defun set-cat-entry (char newvalue &optional (rt *readtable*))
  (declare (character char) (type (unsigned-byte 8) newvalue) (readtable rt))
  (if (typep char 'base-char)
      (setf (elt (character-attribute-array rt) (char-code char)) newvalue)
      (if (= newvalue +char-attr-constituent+)
          ;; Default value for the C-A-HASH-TABLE is +CHAR-ATTR-CONSTITUENT+.
          (%remhash char (character-attribute-hash-table rt))
          (setf (gethash char (character-attribute-hash-table rt)) newvalue)))
  (values))

;; Set the character-macro-table entry without coercing NEW-VALUE.
;; As used by set-syntax-from-char it must always process "raw" values.
(defun set-cmt-entry (char new-value &optional (rt *readtable*))
  (declare (character char)
           (type (or null function fdefn) new-value)
           (type readtable rt))
  (if (typep char 'base-char)
      (setf (svref (character-macro-array rt) (char-code char)) new-value)
      (if new-value ; never store NILs
          (setf (gethash char (character-macro-hash-table rt)) new-value)
          (remhash char (character-macro-hash-table rt)))))

;;; the value actually stored in the character macro table. As per
;;; ANSI #'GET-MACRO-CHARACTER and #'SET-MACRO-CHARACTER, this can
;;; be either a function-designator or NIL, except that we store
;;; symbols not as themselves but as their #<fdefn>.
(defun get-raw-cmt-entry (char readtable)
  (declare (character char) (readtable readtable))
  (if (typep char 'base-char)
      (svref (character-macro-array readtable) (char-code char))
      (values (gethash char (character-macro-hash-table readtable) nil))))

;; As above but get the entry for SUB-CHAR in a dispatching macro table.
(defun get-raw-cmt-dispatch-entry (sub-char sub-table)
  (declare (character sub-char))
  (if (typep sub-char 'base-char)
      (svref (truly-the (simple-vector #.base-char-code-limit)
                        (cdr (truly-the cons sub-table)))
             (char-code sub-char))
      (awhen (car sub-table)
        (gethash sub-char it))))

;; Coerce THING to a character-macro-table entry
(defmacro !coerce-to-cmt-entry (thing)
  `(let ((x ,thing))
     (if (typep x '(or null function)) x (find-or-create-fdefn x))))

;; Return a callable function given a character-macro-table entry.
(defmacro !cmt-entry-to-function (val fallback)
  `(let ((x ,val))
     (truly-the
      function
      (cond ((functionp x) x)
            ((null x) ,fallback)
            (t (sb!c:safe-fdefn-fun x))))))

;; Return a function-designator given a character-macro-table entry.
(defmacro !cmt-entry-to-fun-designator (val)
  `(let ((x ,val))
     (if (fdefn-p x) (fdefn-name x) x)))

;;; The character attribute table is a BASE-CHAR-CODE-LIMIT vector
;;; of (unsigned-byte 8) plus a hashtable to handle higher character codes.

(defmacro test-attribute (char whichclass rt)
  `(= (get-cat-entry ,char ,rt) ,whichclass))

;;; predicates for testing character attributes

#!-sb-fluid
(progn
  (declaim (inline whitespace[1]p whitespace[2]p))
  (declaim (inline constituentp terminating-macrop))
  (declaim (inline single-escape-p multiple-escape-p))
  (declaim (inline token-delimiterp)))

;;; the [1] and [2] here refer to ANSI glossary entries for
;;; "whitespace".
;; whitespace[2]p is the only predicate whose readtable is optional
;; - other than whitespace[1]p which has a fixed readtable - due to
;; callers not otherwise needing a readtable at all, and so not binding
;; *READTABLE* into a local variable throughout their lifetime.
(defun whitespace[1]p (char)
  (test-attribute char +char-attr-whitespace+ *standard-readtable*))
(defun whitespace[2]p (char &optional (rt *readtable*))
  (test-attribute char +char-attr-whitespace+ rt))

(defun constituentp (char rt)
  (test-attribute char +char-attr-constituent+ rt))

(defun terminating-macrop (char rt)
  (test-attribute char +char-attr-terminating-macro+ rt))

(defun single-escape-p (char rt)
  (test-attribute char +char-attr-single-escape+ rt))

(defun multiple-escape-p (char rt)
  (test-attribute char +char-attr-multiple-escape+ rt))

(defun token-delimiterp (char &optional (rt *readtable*))
  ;; depends on actual attribute numbering in readtable.lisp.
  (<= (get-cat-entry char rt) +char-attr-terminating-macro+))

;;;; constituent traits (see ANSI 2.1.4.2)

;;; There are a number of "secondary" attributes which are constant
;;; properties of characters (as long as they are constituents).

(declaim (type attribute-table *constituent-trait-table*))
(defglobal *constituent-trait-table*
  (make-array base-char-code-limit
              :element-type '(unsigned-byte 8)
              :initial-element +char-attr-constituent+))

(defun !set-constituent-trait (char trait)
  (aver (typep char 'base-char))
  (setf (elt *constituent-trait-table* (char-code char))
        trait))

(defun !cold-init-constituent-trait-table ()
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
  ;; This function does not accept a readtable designator, only a readtable.
  (assert-not-standard-readtable readtable '(setf readtable-case))
  (setf (%readtable-case readtable) case))

(defun readtable-normalization (readtable)
  #!+sb-doc
  "Returns T if READTABLE normalizes strings to NFKC, and NIL otherwise.
The READTABLE-NORMALIZATION of the standard readtable is T."
  (%readtable-normalization readtable))

(defun (setf readtable-normalization) (new-value readtable)
  #!+sb-doc
  "Sets the READTABLE-NORMALIZATION of the given READTABLE to NEW-VALUE.
Pass T to make READTABLE normalize symbols to NFKC (the default behavior),
and NIL to suppress normalization."
  ;; This function does not accept a readtable designator, only a readtable.
  (assert-not-standard-readtable readtable '(setf readtable-normalization))
  (setf (%readtable-normalization readtable) new-value))

(defun replace/eql-hash-table (to from &optional (transform #'identity))
  (maphash (lambda (k v) (setf (gethash k to) (funcall transform v))) from)
  to)

(defun %make-dispatch-macro-char (dtable)
  (lambda (stream char)
    (declare (ignore char))
    (read-dispatch-char stream dtable)))

(defun %dispatch-macro-char-table (fun)
  (and (closurep fun)
       (eq (%closure-fun fun)
           (load-time-value (%closure-fun (%make-dispatch-macro-char nil))
                            t))
       (find-if-in-closure #'consp fun)))

;; If ENTRY is a dispatching macro, copy its dispatch table.
;; Otherwise return it without alteration.
(defun copy-cmt-entry (entry)
  (let ((dtable (%dispatch-macro-char-table entry)))
    (if dtable
        (%make-dispatch-macro-char
         (cons (awhen (car dtable)
                 (replace/eql-hash-table (make-hash-table) it))
               (copy-seq (cdr dtable))))
        entry)))

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (assert-not-standard-readtable to-readtable 'copy-readtable)
  (let ((really-from-readtable (or from-readtable *standard-readtable*))
        (really-to-readtable (or to-readtable (make-readtable))))
    (replace (character-attribute-array really-to-readtable)
             (character-attribute-array really-from-readtable))
    (replace/eql-hash-table
     (character-attribute-hash-table really-to-readtable)
     (character-attribute-hash-table really-from-readtable))
    ;; CLHS says that when TO-READTABLE is non-nil "... the readtable specified
    ;; ... is modified and returned." Is that to imply making TO-READTABLE look
    ;; exactly like FROM-READTABLE, or does it mean to augment it?
    ;; We have conflicting behaviors - everything in the base-char range,
    ;; is overwritten, but above that range it's additive.
    (map-into (character-macro-array really-to-readtable)
              #'copy-cmt-entry
              (character-macro-array really-from-readtable))
    (replace/eql-hash-table
     (character-macro-hash-table really-to-readtable)
     (character-macro-hash-table really-from-readtable)
     #'copy-cmt-entry)
    (setf (readtable-case really-to-readtable)
          (readtable-case really-from-readtable))
    (setf (readtable-normalization really-to-readtable)
          (readtable-normalization really-from-readtable))
    really-to-readtable))

(defun set-syntax-from-char (to-char from-char &optional
                             (to-readtable *readtable*) (from-readtable nil))
  #!+sb-doc
  "Causes the syntax of TO-CHAR to be the same as FROM-CHAR in the optional
readtable (defaults to the current readtable). The FROM-TABLE defaults to the
standard Lisp readtable when NIL."
  ;; TO-READTABLE is a readtable, not a readtable-designator
  (assert-not-standard-readtable to-readtable 'set-syntax-from-char)
  (let* ((really-from-readtable (or from-readtable *standard-readtable*))
         (att (get-cat-entry from-char really-from-readtable))
         (mac (get-raw-cmt-entry from-char really-from-readtable)))
    (set-cat-entry to-char att to-readtable)
    (set-cmt-entry to-char (copy-cmt-entry mac) to-readtable))
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
    (set-cmt-entry char (!coerce-to-cmt-entry function) designated-readtable)
    t)) ; (ANSI-specified return value)

(defun get-macro-character (char &optional (rt-designator *readtable*))
  #!+sb-doc
  "Return the function associated with the specified CHAR which is a macro
  character, or NIL if there is no such function. As a second value, return
  T if CHAR is a macro character which is non-terminating, i.e. which can
  be embedded in a symbol name."
  (let* ((designated-readtable (or rt-designator *standard-readtable*))
         ;; the first return value: (OR FUNCTION SYMBOL) if CHAR is a macro
         ;; character, or NIL otherwise
         (fun-value (!cmt-entry-to-fun-designator
                     (get-raw-cmt-entry char designated-readtable))))
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

(defun get-dispatch-macro-char-table (disp-char readtable &optional (errorp t))
  (cond ((%dispatch-macro-char-table (get-raw-cmt-entry disp-char readtable)))
        (errorp (error "~S is not a dispatching macro character." disp-char))))

(defun make-dispatch-macro-character (char &optional
                                      (non-terminating-p nil)
                                      (rt *readtable*))
  #!+sb-doc
  "Cause CHAR to become a dispatching macro character in readtable (which
   defaults to the current readtable). If NON-TERMINATING-P, the char will
   be non-terminating."
  ;; This used to call ERROR if the character was already a dispatching
  ;; macro but I saw no evidence of that in other implementations except cmucl.
  ;; Without a portable way to inquire whether a character is dispatching,
  ;; a file that frobs *READTABLE* can't be repeatedly loaded except
  ;; by catching the error, so I removed it.
  ;; RT is a readtable, not a readtable-designator, as per CLHS.
  (unless (get-dispatch-macro-char-table char rt nil)
    ;; The dtable is a cons whose whose CAR is initially NIL but upgraded
    ;; to a hashtable if required, and whose CDR is a vector indexed by
    ;; char-code up to the maximum base-char.
    (let ((dtable (cons nil (make-array base-char-code-limit
                                        :initial-element nil))))
      (set-macro-character char (%make-dispatch-macro-char dtable)
                           non-terminating-p rt)))
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
    (let ((dtable (get-dispatch-macro-char-table disp-char readtable))
          (function (!coerce-to-cmt-entry function)))
      ;; (SET-MACRO-CHARACTER #\$ (GET-MACRO-CHARACTER #\#)) will share
      ;; the dispatch table. Perhaps it should be copy-on-write?
      (if (typep sub-char 'base-char)
          (setf (svref (cdr dtable) (char-code sub-char)) function)
          (let ((hashtable (car dtable)))
            (cond (function ; allocate the hashtable if it wasn't made yet
                   (setf (gethash sub-char
                                  (or hashtable (setf (car dtable)
                                                      (make-hash-table))))
                         function))
                  (hashtable ; remove an existing entry
                   (remhash sub-char hashtable)))))))
  t)

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (rt-designator *readtable*))
  #!+sb-doc
  "Return the macro character function for SUB-CHAR under DISP-CHAR
   or NIL if there is no associated function."
  (let ((dtable (get-dispatch-macro-char-table
                 disp-char (or rt-designator *standard-readtable*))))
    (!cmt-entry-to-fun-designator
     (get-raw-cmt-dispatch-entry (char-upcase sub-char) dtable))))


;;;; definitions to support internal programming conventions

(defconstant +EOF+ 0)

(defun flush-whitespace (stream)
  ;; This flushes whitespace chars, returning the last char it read (a
  ;; non-white one). It always gets an error on end-of-file.
  (let* ((stream (in-synonym-of stream))
         (rt *readtable*)
         (attribute-array (character-attribute-array rt))
         (attribute-hash-table (character-attribute-hash-table rt)))
    (macrolet ((done-p ()
                '(not (eql (if (typep char 'base-char)
                               (aref attribute-array (char-code char))
                               (gethash char attribute-hash-table
                                        +char-attr-constituent+))
                           +char-attr-whitespace+))))
      (if (ansi-stream-p stream)
          (prepare-for-fast-read-char stream
            (loop (let ((char (fast-read-char t)))
                    (cond ((done-p)
                           (done-with-fast-read-char)
                           (return char))))))
        ;; CLOS stream
          (loop (let ((char (read-char stream nil +EOF+)))
                  ;; (THE) should not be needed if DONE-P, but it was not
                  ;; being derived to return a character, causing an extra
                  ;; check in consumers of flush-whitespace despite the
                  ;; promise to return a character or else signal EOF.
                  (cond ((eq char +EOF+) (error 'end-of-file :stream stream))
                        ((done-p) (return (the character char))))))))))

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
  ;; Using symbols makes these traceable and redefineable with ease,
  ;; as well as avoids a forward-referenced function (from "backq")
  (set-macro-character #\( 'read-list)
  (set-macro-character #\) 'read-right-paren)
  (set-macro-character #\; #'read-comment)
  ;; (The hairier macro-character definitions, for #\# and #\`, are
  ;; defined elsewhere, in their own source files.)

  ;; all constituents
  (do ((ichar 0 (1+ ichar))
       (char))
      ((= ichar base-char-code-limit))
    (setq char (code-char ichar))
    (when (constituentp char *readtable*)
      (set-cmt-entry char nil)))

  (/show0 "leaving !cold-init-standard-readtable"))

;;;; implementation of the read buffer

(defstruct (token-buf (:predicate nil) (:copier nil)
                      (:constructor
                       make-token-buf
                       (&aux
                        (initial-string (make-string 128))
                        (string initial-string)
                        (adjustable-string
                         (make-array 0
                                     :element-type 'character
                                     :fill-pointer nil
                                     :displaced-to string)))))
  ;; The string accumulated during reading of tokens.
  ;; Always starts out EQ to 'initial-string'.
  (string nil :type (simple-array character (*)))
  ;; Counter advanced as characters are placed into 'string'
  (fill-ptr 0 :type index)
  ;; Counter advanced as characters are consumed from 'string' on re-scan
  ;; by auxilliary functions MAKE-{INTEGER,FLOAT,RATIONAL} etc.
  (cursor 0 :type index)
  ;; A string used only for FIND-PACKAGE calls in package-qualified
  ;; symbols so that we don't need to call SUBSEQ on the 'string'.
  (adjustable-string nil :type (and (array character (*)) (not simple-array)))
  ;; A small string that is permanently assigned into this token-buf.
  (initial-string nil :type (simple-array character (128))
                  :read-only t)
  (escapes (make-array 10 :element-type 'fixnum :fill-pointer 0 :adjustable t)
           :type (and (vector fixnum) (not simple-array)) :read-only t)
  ;; Link to next TOKEN-BUF, to chain the *TOKEN-BUF-POOL* together.
  (next nil :type (or null token-buf))
  (only-base-chars t :type boolean))
(declaim (freeze-type token-buf))

(defmethod print-object ((self token-buf) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~@[next=~S~]" (token-buf-next self))))

;; The current TOKEN-BUF
(declaim (type token-buf *read-buffer*))
(defvar *read-buffer*)

;; A list of available TOKEN-BUFs
;; Should need no toplevel binding if multi-threaded,
;; but doesn't really matter, as INITIAL-THREAD-FUNCTION-TRAMPOLINE
;; rebinds to NIL.
(declaim (type (or null token-buf) *token-buf-pool*))
(defvar *token-buf-pool* nil)

(defun reset-read-buffer (buffer)
  ;; Turn BUFFER into an empty read buffer.
  (setf (fill-pointer (token-buf-escapes buffer)) 0)
  (setf (token-buf-fill-ptr buffer) 0)
  (setf (token-buf-cursor buffer) 0)
  (setf (token-buf-only-base-chars buffer) t)
  buffer)

;; "Output" a character into the reader's buffer.
(declaim (inline ouch-read-buffer))
(defun ouch-read-buffer (char buffer)
  ;; When buffer overflow
  (let ((op (token-buf-fill-ptr buffer)))
    (declare (optimize (sb!c::insert-array-bounds-checks 0)))
    (when (>= op (length (token-buf-string buffer)))
    ;; an out-of-line call for the uncommon case avoids bloat.
    ;; Size should be doubled.
      (grow-read-buffer))
    (unless (typep char 'base-char)
      (setf (token-buf-only-base-chars buffer) nil))
    (setf (elt (token-buf-string buffer) op) char)
    (setf (token-buf-fill-ptr buffer) (1+ op))))

(defun ouch-read-buffer-escaped (char buf)
  (vector-push-extend (token-buf-fill-ptr buf) (token-buf-escapes buf))
  (ouch-read-buffer char buf))

(defun grow-read-buffer ()
  (let* ((b *read-buffer*)
         (string (token-buf-string b)))
    (setf (token-buf-string b)
          (replace (make-string (* 2 (length string))) string))))

;; Retun the next character from the buffered token, or NIL.
(declaim (maybe-inline token-buf-getchar))
(defun token-buf-getchar (b)
  (declare (optimize (sb!c::insert-array-bounds-checks 0)))
  (let ((i (token-buf-cursor (truly-the token-buf b))))
    (and (< i (token-buf-fill-ptr b))
         (prog1 (elt (token-buf-string b) i)
           (setf (token-buf-cursor b) (1+ i))))))

;; Grab a buffer off the token-buf pool if there is one, or else make one.
;; This does not need to be protected against other threads because the
;; pool is thread-local, or against async interrupts. An async signal
;; delivered anywhere in the midst of the code sequence below can not
;; corrupt the buffer given to the caller of ACQUIRE-TOKEN-BUF.
;; Additionally the cleanup is on a "best effort" basis. Async unwinds
;; through WITH-READ-BUFFER fail to recycle token-bufs, but that's ok.
(defun acquire-token-buf ()
  (let ((this-buffer *token-buf-pool*))
    (cond (this-buffer
           (shiftf *token-buf-pool* (token-buf-next this-buffer) nil)
           this-buffer)
          (t
           (make-token-buf)))))

(defun release-token-buf (chain)
  (named-let free ((buffer chain))
    ;; If 'adjustable-string' was displaced to 'string',
    ;; adjust it back down to allow GC of the abnormally large string.
    (unless (eq (%array-data-vector (token-buf-adjustable-string buffer))
                (token-buf-initial-string buffer))
      (adjust-array (token-buf-adjustable-string buffer) '(0)
                    :displaced-to (token-buf-initial-string buffer)))
    ;; 'initial-string' is assigned into 'string'
    ;; so not to preserve huge buffers in the pool indefinitely.
    (setf (token-buf-string buffer) (token-buf-initial-string buffer))
    (if (token-buf-next buffer)
        (free (token-buf-next buffer))
        (setf (token-buf-next buffer) *token-buf-pool*)))
  (setf *token-buf-pool* chain))

;; Return a fresh copy of BUFFER's string
(defun copy-token-buf-string (buffer)
  (subseq (token-buf-string buffer) 0 (token-buf-fill-ptr buffer)))

;; Return a string displaced to BUFFER's string.
;; The string should not be held onto - either a copy must be made
;; by the receiver, or it should be parsed into something else.
(defun sized-token-buf-string (buffer)
    ;; It would in theory be faster to make the adjustable array have
    ;; a fill-pointer, and just set that most of the time. Except we still
    ;; need the ability to displace to a different string if a package name
    ;; has >128 characters, so then there'd be two modes of sharing, one of
    ;; which is rarely exercised and most likely to be subtly wrong.
    ;; At any rate, SET-ARRAY-HEADER is faster than ADJUST-ARRAY.
    ;; TODO: find evidence that it is/is-not worth having complicated
    ;;       mechanism involving a fill-pointer or not.
    (set-array-header
     (token-buf-adjustable-string buffer) ; the array
     (token-buf-string buffer) ; the underlying data
     (token-buf-fill-ptr buffer) ; total size
     nil ; fill-pointer
     0 ; displacement
     (token-buf-fill-ptr buffer) ; dimension 0
     t nil)) ; displacedp / newp

;; Acquire a TOKEN-BUF from the pool and execute the body, returning only
;; the primary value therefrom. Recycle the buffer when done.
;; No UNWIND-PROTECT - recycling is designed to help with the common case
;; of normal return and is not intended to be resilient against nonlocal exit.
(defmacro with-read-buffer (() &body body)
  `(let* ((*read-buffer* (acquire-token-buf))
          (result (progn ,@body)))
     (release-token-buf *read-buffer*)
     result))

(defun check-for-recursive-read (stream recursive-p operator-name)
  (when (and recursive-p (not (boundp '*read-buffer*)))
    (simple-reader-error
     stream
     "~A was invoked with RECURSIVE-P being true outside ~
      of a recursive read operation."
     `(,operator-name))))

;;;; READ-PRESERVING-WHITESPACE, READ-DELIMITED-LIST, and READ

;;; A list for #=, used to keep track of objects with labels assigned that
;;; have been completely read. Each entry is a SHARP-EQUAL-WRAPPER object.
;;;
;;; KLUDGE: Should this really be a list? It seems as though users
;;; could reasonably expect N log N performance for large datasets.
;;; On the other hand, it's probably very very seldom a problem in practice.
;;; On the third hand, it might be just as easy to use a hash table,
;;; so maybe we should. -- WHN 19991202
(defvar *sharp-equal* ())

(declaim (ftype (sfunction (t t) (values bit t)) read-maybe-nothing))

;;; Like READ-PRESERVING-WHITESPACE, but doesn't check the read buffer
;;; for being set up properly.
(defun %read-preserving-whitespace (stream eof-error-p eof-value recursive-p)
  (declare (optimize (sb!c::check-tag-existence 0)))
  (if recursive-p
      ;; a loop for repeating when a macro returns nothing
      (let* ((tracking-p (form-tracking-stream-p stream))
             (outermost-p
              (and tracking-p
                   (null (form-tracking-stream-form-start-char-pos stream)))))
        (loop
         (let ((char (read-char stream eof-error-p +EOF+)))
           (cond ((eq char +EOF+) (return eof-value))
                 ((whitespace[2]p char))
                 (t
                  (when outermost-p
                    ;; Calling FILE-POSITION at each token seems to slow down
                    ;; the reader by somewhere between 8x to 10x.
                    ;; Once per outermost form is acceptably fast though.
                    (setf (form-tracking-stream-form-start-byte-pos stream)
                          ;; pretend we queried the position before reading CHAR
                          (- (file-position stream)
                             (or (file-string-length stream (string char)) 0))
                          (form-tracking-stream-form-start-char-pos stream)
                          ;; likewise
                          (1- (form-tracking-stream-input-char-pos stream))))
                  (multiple-value-bind (result-p result)
                      (read-maybe-nothing stream char)
                    (unless (zerop result-p)
                      (return (unless *read-suppress* result)))
                    ;; Repeat if macro returned nothing.
                    (when tracking-p
                      (funcall (form-tracking-stream-observer stream)
                               :reset nil nil))))))))
      (let ((*sharp-equal* nil))
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
  (declare (explicit-check))
  (check-for-recursive-read stream recursive-p 'read-preserving-whitespace)
  (%read-preserving-whitespace stream eof-error-p eof-value recursive-p))

;;; Read from STREAM given starting CHAR, returning 1 and the resulting
;;; object, unless CHAR is a macro yielding no value, then 0 and NIL,
;;; for functions that want comments to return so that they can look
;;; past them. CHAR must not be whitespace.
(defun read-maybe-nothing (stream char)
  (truly-the
   (values bit t) ; avoid a type-check. M-V-CALL is lame
   (multiple-value-call
       (lambda (stream start-pos &optional (result nil supplied-p) &rest junk)
         (declare (ignore junk)) ; is this ANSI-specified?
         (when (and supplied-p start-pos)
           (funcall (form-tracking-stream-observer stream)
                    start-pos
                    (form-tracking-stream-input-char-pos stream) result))
         (values (if supplied-p 1 0) result))
     ;; KLUDGE: not capturing anything in the lambda avoids closure consing
     stream
     (and (form-tracking-stream-p stream)
          ;; Subtract 1 because the position points _after_ CHAR.
          (1- (form-tracking-stream-input-char-pos stream)))
     (funcall (!cmt-entry-to-function
               (get-raw-cmt-entry char *readtable*) #'read-token)
              stream char))))

(defun read (&optional (stream *standard-input*)
                       (eof-error-p t)
                       (eof-value nil)
                       (recursive-p nil))
  #!+sb-doc
  "Read the next Lisp value from STREAM, and return it."
  (declare (explicit-check))
  (check-for-recursive-read stream recursive-p 'read)
  (let* ((local-eof-val (load-time-value (cons nil nil) t))
         (result (%read-preserving-whitespace
                  stream eof-error-p local-eof-val recursive-p)))
    ;; This function generally discards trailing whitespace. If you
    ;; don't want to discard trailing whitespace, call
    ;; CL:READ-PRESERVING-WHITESPACE instead.
    (unless (or (eql result local-eof-val) recursive-p)
      (let ((next-char (read-char stream nil +EOF+)))
        (unless (or (eq next-char +EOF+)
                    (whitespace[2]p next-char))
          (unread-char next-char stream))))
    (if (eq result local-eof-val) eof-value result)))


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
           (loop (let ((char (fast-read-char nil +EOF+)))
                   (when (or (eq char +EOF+) (char= char #\newline))
                     (return (done-with-fast-read-char))))))
          ;; CLOS stream
          (loop (let ((char (read-char stream nil +EOF+)))
                  (when (or (eq char +EOF+) (char= char #\newline))
                    (return)))))))
  ;; Don't return anything.
  (values))

;;; FIXME: for these two macro chars, if STREAM is a FORM-TRACKING-STREAM,
;;; every cons cell should generate a notification so that the readtable
;;; manipulation in SB-COVER can be eliminated in favor of a stream observer.
;;; It is cheap to add events- it won't increase consing in the compiler
;;; because it the extra events can simply be ignored.
(macrolet
    ((with-list-reader ((streamvar delimiter) &body body)
       `(let* ((thelist (list nil))
               (listtail thelist)
               (collectp (if *read-suppress* 0 -1)))
          (declare (dynamic-extent thelist))
          (loop (let ((firstchar (flush-whitespace ,streamvar)))
                  (when (eq firstchar ,delimiter)
                    (return (cdr thelist)))
                  ,@body))))
     (read-list-item (streamvar)
       `(multiple-value-bind (winp obj)
            (read-maybe-nothing ,streamvar firstchar)
          ;; allow for a character macro return to return nothing
          (unless (zerop (logand winp collectp))
            (setq listtail
                  (cdr (rplacd (truly-the cons listtail) (list obj))))))))

  ;;; The character macro handler for left paren
  (defun read-list (stream ignore)
    (declare (ignore ignore))
    (with-list-reader (stream #\))
      (when (eq firstchar #\.)
        (let ((nextchar (read-char stream t)))
          (cond ((token-delimiterp nextchar)
                 (cond ((eq listtail thelist)
                        (unless (zerop collectp)
                          (simple-reader-error
                           stream "Nothing appears before . in list.")))
                       ((whitespace[2]p nextchar)
                        (setq nextchar (flush-whitespace stream))))
                 (rplacd (truly-the cons listtail)
                         (read-after-dot stream nextchar collectp))
                 ;; Check for improper ". ,@" or ". ,." now rather than
                 ;; in the #\` reader. The resulting QUASIQUOTE macro might
                 ;; never be exapanded, but nonetheless could be erroneous.
                 (unless (zerop (logand *backquote-depth* collectp))
                   (let ((lastcdr (cdr (last listtail))))
                     (when (and (comma-p lastcdr) (comma-splicing-p lastcdr))
                       (simple-reader-error
                        stream "~S contains a splicing comma after a dot"
                        (cdr thelist)))))
                 (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                (t (unread-char nextchar stream)))))
      ;; Next thing is not an isolated dot.
      (read-list-item stream)))

  ;;; (This is a COMMON-LISP exported symbol.)
  (defun read-delimited-list (endchar &optional
                                      (input-stream *standard-input*)
                                      recursive-p)
  #!+sb-doc
  "Read Lisp values from INPUT-STREAM until the next character after a
   value's representation is ENDCHAR, and return the objects as a list."
    (declare (explicit-check))
    (check-for-recursive-read input-stream recursive-p 'read-delimited-list)
    (flet ((%read-delimited-list ()
             (with-list-reader (input-stream endchar)
               (read-list-item input-stream))))
      (if recursive-p
          (%read-delimited-list)
          (with-read-buffer () (%read-delimited-list)))))) ; end MACROLET

(defun read-after-dot (stream firstchar collectp)
  ;; FIRSTCHAR is non-whitespace!
  (let ((lastobj ()))
    (do ((char firstchar (flush-whitespace stream)))
        ((eq char #\))
         (if (zerop collectp)
             (return-from read-after-dot nil)
             (simple-reader-error stream "Nothing appears after . in list.")))
      ;; See whether there's something there.
      (multiple-value-bind (winp obj) (read-maybe-nothing stream char)
        (unless (zerop winp) (return (setq lastobj obj)))))
    ;; At least one thing appears after the dot.
    ;; Check for more than one thing following dot.
    (loop
     (let ((char (flush-whitespace stream)))
       (cond ((eq char #\)) (return lastobj)) ;success!
             ;; Try reading virtual whitespace.
             ((not (zerop (logand (read-maybe-nothing stream char)
                                  (truly-the fixnum collectp))))
              (simple-reader-error
               stream "More than one object follows . in list.")))))))

(defun read-string (stream closech)
  ;; This accumulates chars until it sees same char that invoked it.
  ;; We avoid copying any given input character more than twice-
  ;; once to a temp buffer and then to the result. In the worst case,
  ;; we can waste space equal the unwasted space, if the final character
  ;; causes allocation of a new buffer for just that character,
  ;; because the buffer size is doubled each time it overflows.
  ;; (Would be better to peek at the frc-buffer if the stream has one.)
  ;; Scratch vectors are GC-able as soon as this function returns though.
  (declare (character closech))
  (macrolet ((scan (read-a-char eofp &optional finish)
               `(loop (let ((char ,read-a-char))
                        (declare (optimize (sb!c::insert-array-bounds-checks 0)))
                        (cond (,eofp (error 'end-of-file :stream stream))
                              ((eql char closech)
                               (return ,finish))
                              ((single-escape-p char rt)
                               (setq char ,read-a-char)
                               (when ,eofp
                                 (error 'end-of-file :stream stream))))
                        (when (>= ptr lim)
                          (unless suppress
                            (push buf chain)
                            (setq lim (the index (ash lim 1))
                                  buf (make-array lim :element-type 'character)))
                          (setq ptr 0))
                        (setf (schar buf ptr) (truly-the character char))
                        (incf ptr)))))
    (let* ((token-buf *read-buffer*)
           (buf (token-buf-string token-buf))
           (rt *readtable*)
           (stream (in-synonym-of stream))
           (suppress *read-suppress*)
           (lim (length buf))
           (ptr 0)
           (chain))
      (declare (type (simple-array character (*)) buf))
      (reset-read-buffer token-buf)
      (if (ansi-stream-p stream)
          (prepare-for-fast-read-char stream
           (scan (fast-read-char t) nil (done-with-fast-read-char)))
          ;; CLOS stream
          (scan (read-char stream nil +EOF+) (eq char +EOF+)))
      (if suppress
          ""
          (let* ((sum (loop for buf in chain sum (length buf)))
                 (result (make-array (+ sum ptr) :element-type 'character)))
            (setq ptr sum)
            ;; Now work backwards from the end
            (replace result buf :start1 ptr)
            (dolist (buf chain result)
              (declare (type (simple-array character (*)) buf))
              (let ((len (length buf)))
                (decf ptr len)
                (replace result buf :start1 ptr))))))))

(defun read-right-paren (stream ignore)
  (declare (ignore ignore))
  (simple-reader-error stream "unmatched close parenthesis"))

;;; Read from the stream up to the next delimiter. Leave the resulting
;;; token in *READ-BUFFER*, and return three values:
;;; -- a TOKEN-BUF
;;; -- whether any escape character was seen (even if no character is escaped)
;;; -- whether a package delimiter character was seen
;;; Normalizes the input to NFKC before returning
(defun internal-read-extended-token (stream firstchar escape-firstchar
                                     &aux (read-buffer *read-buffer*))
  (reset-read-buffer read-buffer)
  (when escape-firstchar
    (ouch-read-buffer-escaped firstchar read-buffer)
    (setq firstchar (read-char stream nil +EOF+)))
  (do ((char firstchar (read-char stream nil +EOF+))
       (seen-multiple-escapes nil)
       (rt *readtable*)
       (colon nil))
      ((cond ((eq char +EOF+) t)
             ((token-delimiterp char rt)
              (unread-char char stream)
              t)
             (t nil))
       (progn
         (multiple-value-setq (read-buffer colon)
           (normalize-read-buffer read-buffer colon))
         (values read-buffer
                 (or (plusp (fill-pointer (token-buf-escapes read-buffer)))
                     seen-multiple-escapes)
                 colon)))
    (flet ((escape-1-char ()
             ;; It can't be a number, even if it's 1\23.
             ;; Read next char here, so it won't be casified.
             (let ((nextchar (read-char stream nil +EOF+)))
               (if (eq nextchar +EOF+)
                   (reader-eof-error stream "after escape character")
                   (ouch-read-buffer-escaped nextchar read-buffer)))))
      (cond ((single-escape-p char rt) (escape-1-char))
            ((multiple-escape-p char rt)
             (setq seen-multiple-escapes t)
             ;; Read to next multiple-escape, escaping single chars
             ;; along the way.
             (loop
              (let ((ch (read-char stream nil +EOF+)))
                (cond ((eq ch +EOF+)
                       (reader-eof-error stream "inside extended token"))
                      ((multiple-escape-p ch rt) (return))
                      ((single-escape-p ch rt) (escape-1-char))
                      (t (ouch-read-buffer-escaped ch read-buffer))))))
            (t
             (when (and (not colon) ; easiest test first
                        (constituentp char rt)
                        (eql (get-constituent-trait char)
                             +char-attr-package-delimiter+))
               (setq colon t))
             (ouch-read-buffer char read-buffer))))))

;;;; character classes

;;; Return the character class for CHAR.
;;;
;;; FIXME: why aren't these ATT-getting forms using GET-CAT-ENTRY?
;;; Because we've cached the readtable tables?
(defmacro char-class (char attarray atthash)
  `(let ((att (if (typep (truly-the character ,char) 'base-char)
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
  `(let ((att (if (typep (truly-the character ,char) 'base-char)
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
  `(let ((att (if (typep (truly-the character ,char) 'base-char)
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

;;; Normalize TOKEN-BUF to NFKC, returning a new TOKEN-BUF and the
;;; COLON value
(defun normalize-read-buffer (token-buf &optional colon)
  (unless (readtable-normalization *readtable*)
    (return-from normalize-read-buffer (values token-buf colon)))
  (when (token-buf-only-base-chars token-buf)
    (return-from normalize-read-buffer (values token-buf colon)))
  (let ((current-buffer (copy-token-buf-string token-buf))
        (old-escapes (copy-seq (token-buf-escapes token-buf)))
        (str-to-normalize (make-string (token-buf-fill-ptr token-buf)))
        (normalize-ptr 0) (escapes-ptr 0))
    (reset-read-buffer token-buf)
    (macrolet ((clear-str-to-normalize ()
               `(progn
                  (loop for char across (sb!unicode:normalize-string
                                         (subseq str-to-normalize 0 normalize-ptr)
                                         :nfkc) do
                       (ouch-read-buffer char token-buf))
                  (setf normalize-ptr 0)))
               (push-to-normalize (ch)
                 (let ((ch-gen (gensym)))
                   `(let ((,ch-gen ,ch))
                      (setf (char str-to-normalize normalize-ptr) ,ch-gen)
                      (incf normalize-ptr)))))
      (loop for c across current-buffer
         for i from 0
         do
           (if (and (< escapes-ptr (length old-escapes))
                    (eql i (aref old-escapes escapes-ptr)))
               (progn
                 (clear-str-to-normalize)
                 (ouch-read-buffer-escaped c token-buf)
                 (incf escapes-ptr))
               (push-to-normalize c)))
      (clear-str-to-normalize)
      (values token-buf colon))))

;;; Modify the read buffer according to READTABLE-CASE, ignoring
;;; ESCAPES. ESCAPES is a vector of the escaped indices.
(defun casify-read-buffer (token-buf)
  (let ((case (readtable-case *readtable*))
        (escapes (token-buf-escapes token-buf)))
    (cond
     ((and (zerop (length escapes)) (eq case :upcase))
      (let ((buffer (token-buf-string token-buf)))
        (dotimes (i (token-buf-fill-ptr token-buf))
          (declare (optimize (sb!c::insert-array-bounds-checks 0)))
          (setf (schar buffer i) (char-upcase (schar buffer i))))))
     ((eq case :preserve))
     (t
      (macrolet ((skip-esc (&body body)
                   `(do ((i (1- (token-buf-fill-ptr token-buf)) (1- i))
                         (buffer (token-buf-string token-buf))
                         (esc (if (zerop (fill-pointer escapes))
                                  -1 (vector-pop escapes))))
                        ((minusp i))
                      (declare (fixnum i)
                               (optimize (sb!c::insert-array-bounds-checks 0)))
                      (if (< esc i)
                          (let ((ch (schar buffer i)))
                            ,@body)
                          (progn
                            (aver (= esc i))
                            (setq esc (if (zerop (fill-pointer escapes))
                                          -1 (vector-pop escapes))))))))
        (flet ((lower-em ()
                 (skip-esc (setf (schar buffer i) (char-downcase ch))))
               (raise-em ()
                 (skip-esc (setf (schar buffer i) (char-upcase ch)))))
          (ecase case
            (:upcase (raise-em))
            (:downcase (lower-em))
            (:invert
             (let ((all-upper t)
                   (all-lower t)
                   (fillptr (fill-pointer escapes)))
               (skip-esc
                 (when (both-case-p ch)
                   (if (upper-case-p ch)
                       (setq all-lower nil)
                       (setq all-upper nil))))
               (setf (fill-pointer escapes) fillptr)
               (cond (all-lower (raise-em))
                     (all-upper (lower-em))))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *reader-package* nil))
(declaim (type (or null package) *reader-package*)
         (always-bound *reader-package*))

(defun reader-find-package (package-designator stream)
  (if (%instancep package-designator)
      package-designator
      (let ((package (find-package package-designator)))
        (cond (package
               ;; Release the token-buf that was used for the designator
               (release-token-buf (shiftf (token-buf-next *read-buffer*) nil))
               package)
              (t
               (error 'simple-reader-package-error
                      :package package-designator
                      :stream stream
                      :format-control "Package ~A does not exist."
                      :format-arguments (list package-designator)))))))

(defun read-token (stream firstchar)
  #!+sb-doc
  "Default readmacro function. Handles numbers, symbols, and SBCL's
extended <package-name>::<form-in-package> syntax."
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
  (let* ((rt *readtable*)
         (attribute-array (character-attribute-array rt))
         (attribute-hash-table (character-attribute-hash-table rt))
         (buf *read-buffer*)
         (package-designator nil)
         (colons 0)
         (possibly-rational t)
         (seen-digit-or-expt nil)
         (possibly-float t)
         (was-possibly-float nil)
         (seen-multiple-escapes nil))
    (declare (token-buf buf))
    (reset-read-buffer buf)
    (macrolet ((getchar-or-else (what)
                 `(when (eq (setq char (read-char stream nil +EOF+)) +EOF+)
                    ,what)))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-integer)))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-integer)))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-integer 10)))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-constituent-expt+ (go EXPONENT))
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (return (make-integer 10)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RIGHTDIGIT ; saw "[sign] {decimal-digit}* dot {digit}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-float stream)))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RIGHTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (t (go SYMBOL)))
     FRONTDOT ; saw "dot"
      (ouch-read-buffer char buf)
      (getchar-or-else (simple-reader-error stream "dot context error"))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go EXPTDIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     EXPTDIGIT ; got to EXPONENT, saw "[sign] {digit}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-float stream)))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class2 char attribute-array attribute-hash-table)
        (#.+char-attr-constituent-digit+ (go RATIODIGIT))
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
     RATIODIGIT ; saw "[sign] {digit}+ slash {digit}+"
      (ouch-read-buffer char buf)
      (getchar-or-else (return (make-ratio stream)))
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
      (ouch-read-buffer char buf)
      (getchar-or-else (simple-reader-error stream "too many dots"))
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
        (macrolet
           ((scan (read-a-char &optional finish)
             `(prog ()
               SYMBOL-LOOP
               (ouch-read-buffer char buf)
               (setq char ,read-a-char)
               (when (eq char +EOF+) (go RETURN-SYMBOL))
               (case (char-class char attribute-array attribute-hash-table)
                 (#.+char-attr-single-escape+ ,finish (go SINGLE-ESCAPE))
                 (#.+char-attr-delimiter+ ,finish
                                          (unread-char char stream)
                                          (go RETURN-SYMBOL))
                 (#.+char-attr-multiple-escape+ ,finish (go MULT-ESCAPE))
                 (#.+char-attr-package-delimiter+ ,finish (go COLON))
                 (t (go SYMBOL-LOOP))))))
        (if (ansi-stream-p stream)
            (prepare-for-fast-read-char stream
              (scan (fast-read-char nil +EOF+) (done-with-fast-read-char)))
            ;; CLOS stream
            (scan (read-char stream nil +EOF+)))))
     SINGLE-ESCAPE ; saw a single-escape
      ;; Don't put the escape character in the read buffer.
      ;; READ-NEXT CHAR, put in buffer (no case conversion).
      (let ((nextchar (read-char stream nil +EOF+)))
        (when (eq nextchar +EOF+)
          (reader-eof-error stream "after single-escape character"))
        (ouch-read-buffer-escaped nextchar buf))
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      MULT-ESCAPE
      (setq seen-multiple-escapes t)
      ;; sometimes we pass eof-error=nil but check. here we just let it err.
      ;; should pick one style and stick with it.
      (do ((char (read-char stream t) (read-char stream t)))
          ((multiple-escape-p char rt))
        (if (single-escape-p char rt) (setq char (read-char stream t)))
        (ouch-read-buffer-escaped char buf))
      (getchar-or-else (go RETURN-SYMBOL))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+ (unread-char char stream) (go RETURN-SYMBOL))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+ (go COLON))
        (t (go SYMBOL)))
      COLON
      (unless (zerop colons)
        (simple-reader-error
         stream "too many colons in ~S" (copy-token-buf-string buf)))
      (setf buf (normalize-read-buffer buf))
      (casify-read-buffer buf)
      (setq colons 1)
      (setq package-designator
            (if (or (plusp (token-buf-fill-ptr *read-buffer*))
                    seen-multiple-escapes)
                (prog1 (sized-token-buf-string buf)
                  (let ((new (acquire-token-buf)))
                    (setf (token-buf-next new) buf ; new points to old
                          buf new *read-buffer* new)))
                *keyword-package*))
      (reset-read-buffer buf)
      (getchar-or-else (reader-eof-error stream "after reading a colon"))
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
      (getchar-or-else (reader-eof-error stream "after reading a colon"))
      (case (char-class char attribute-array attribute-hash-table)
        (#.+char-attr-delimiter+
         (unread-char char stream)
         (if package-designator
             (let* ((*reader-package*
                     (reader-find-package package-designator stream)))
               (return (read stream t nil t)))
             (simple-reader-error stream
                                  "illegal terminating character after a double-colon: ~S"
                                  char)))
        (#.+char-attr-single-escape+ (go SINGLE-ESCAPE))
        (#.+char-attr-multiple-escape+ (go MULT-ESCAPE))
        (#.+char-attr-package-delimiter+
         (simple-reader-error stream
                              "too many colons after ~S name"
                              package-designator))
        (t (go SYMBOL)))
      RETURN-SYMBOL
      (setf buf (normalize-read-buffer buf))
      (casify-read-buffer buf)
      (let ((pkg (if package-designator
                     (reader-find-package package-designator stream)
                     (or *reader-package* (sane-package)))))
        (if (or (zerop colons) (= colons 2) (eq pkg *keyword-package*))
            (return (%intern (token-buf-string buf) (token-buf-fill-ptr buf)
                             pkg t))
            (multiple-value-bind (symbol accessibility)
                (%find-symbol (token-buf-string buf) (token-buf-fill-ptr buf)
                              pkg)
              (when (eq accessibility :external) (return symbol))
              (let ((name (copy-token-buf-string buf)))
                (with-simple-restart (continue "Use symbol anyway.")
                  (error 'simple-reader-package-error
                         :package pkg
                         :stream stream
                         :format-arguments (list name (package-name pkg))
                         :format-control
                         (if accessibility
                             "The symbol ~S is not external in the ~A package."
                             "Symbol ~S not found in the ~A package.")))
                (return (intern name pkg))))))))))

;;; For semi-external use: Return 3 values: the token-buf,
;;; a flag for whether there was an escape char, and the position of
;;; any package delimiter. The returned token-buf is not case-converted.
(defun read-extended-token (stream)
  ;; recursive-p = T is basically irrelevant.
  (let ((first-char (read-char stream nil +EOF+ t)))
    (if (neq first-char +EOF+)
        (internal-read-extended-token stream first-char nil)
        (values (reset-read-buffer *read-buffer*) nil nil))))

;;; for semi-external use:
;;;
;;; Read an extended token with the first character escaped. Return
;;; the token-buf. The returned token-buf is not case-converted.
(defun read-extended-token-escaped (stream)
  (let ((first-char (read-char stream nil +EOF+)))
    (if (neq first-char +EOF+)
        (values (internal-read-extended-token stream first-char t))
        (reader-eof-error stream "after escape"))))

;;;; number-reading functions

;; Mapping of read-base to the max input characters in a positive fixnum.
(eval-when (:compile-toplevel :execute)
  (defun integer-reader-safe-digits ()
    (do ((a (make-array 35 :element-type '(unsigned-byte 8)))
         (base 2 (1+ base)))
        ((> base 36) a)
      (do ((total (1- base) (+ (* total base) (1- base)))
           (n-digits 0 (1+ n-digits)))
          ((sb!xc:typep total 'bignum)
           (setf (aref a (- base 2)) n-digits))
        ;; empty DO body
        )))

  ;; self-test
  (do ((maxdigits (integer-reader-safe-digits))
       (base 2 (1+ base)))
      ((> base 36))
    (let* ((n-digits (aref maxdigits (- base 2)))
           (d (char (write-to-string (1- base) :base base) 0))
           (string (make-string (1+ n-digits) :initial-element d))) ; 1 extra
      (assert (not (typep (parse-integer string :radix base)
                          `(unsigned-byte ,sb!vm:n-positive-fixnum-bits))))
      (assert (typep (parse-integer string :end n-digits :radix base)
                     `(unsigned-byte ,sb!vm:n-positive-fixnum-bits))))))

(defmacro !setq-optional-leading-sign (sign-flag token-buf rewind)
  ;; guaranteed to have at least one character in buffer at the start
  ;; or immediately following [ESFDL] marker depending on 'rewind' flag.
  `(locally (declare (optimize (sb!c::insert-array-bounds-checks 0)))
     (,(if rewind 'setf 'incf)
       (token-buf-cursor ,token-buf)
       (case (elt (token-buf-string ,token-buf)
                  ,(if rewind 0 `(token-buf-cursor ,token-buf)))
         (#\- (setq ,sign-flag t) 1)
         (#\+ 1)
         (t   0)))))

(defun make-integer (&optional (base *read-base*))
  #!+sb-doc
  "Minimizes bignum-fixnum multiplies by reading a 'safe' number of digits,
  then multiplying by a power of the base and adding."
  (declare ((integer 2 36) base)
           (inline token-buf-getchar)) ; makes for smaller code
  (let* ((fixnum-max-digits
          (macrolet ((maxdigits ()
                       (!coerce-to-specialized  (integer-reader-safe-digits)
                                                '(unsigned-byte 8))))
            (aref (maxdigits) (- base 2))))
         (base-power
          (macrolet ((base-powers ()
                       (do ((maxdigits (integer-reader-safe-digits))
                            (a (make-array 35))
                            (base 2 (1+ base)))
                           ((> base 36) a)
                         (setf (aref a (- base 2))
                               (expt base (aref maxdigits (- base 2)))))))
            (truly-the integer (aref (base-powers) (- base 2)))))
         (negativep nil)
         (result 0)
         (buf *read-buffer*))
    (!setq-optional-leading-sign negativep buf t)
    (loop
     (let ((acc 0))
       (declare (type (and fixnum unsigned-byte) acc))
       (dotimes (digit-count fixnum-max-digits)
         (let ((ch (token-buf-getchar buf)))
           (if (or (not ch) (eql ch #\.))
               (return-from make-integer
                 (let ((result
                        (if (zerop result) acc
                            (+ (* result (expt base digit-count)) acc))))
                   (if negativep (- result) result)))
               (setq acc (truly-the fixnum
                          (+ (digit-char-p ch base)
                             (truly-the fixnum (* acc base))))))))
       (setq result (+ (* result base-power) acc))))))

(defun truncate-exponent (exponent number divisor)
  #!+sb-doc
  "Truncate exponent if it's too large for a float"
  ;; Work with base-2 logarithms to avoid conversions to floats,
  ;; and convert to base-10 conservatively at the end.
  ;; Use the least positive float, because denormalized exponent
  ;; can be larger than normalized.
  (let* ((max-exponent
          #!-long-float
          (+ sb!vm:double-float-digits sb!vm:double-float-bias))
         (number-magnitude (integer-length number))
         (divisor-magnitude (1- (integer-length divisor)))
         (magnitude (- number-magnitude divisor-magnitude)))
    (if (minusp exponent)
        (max exponent (ceiling (- (+ max-exponent magnitude))
                               #.(floor (log 10 2))))
        (min exponent (floor (- max-exponent magnitude)
                             #.(floor (log 10 2)))))))

(defun make-float (stream)
  ;; Assume that the contents of *read-buffer* are a legal float, with nothing
  ;; else after it.
  (let ((buf *read-buffer*)
        (negative-fraction nil)
        (number 0)
        (divisor 1)
        (negative-exponent nil)
        (exponent 0)
        (float-char ())
        char)
    (!setq-optional-leading-sign negative-fraction buf t)
    ;; Read digits before the dot.
    (macrolet ((accumulate (expr)
                 `(let (digit)
                    (loop (if (and (setq char (token-buf-getchar buf))
                                   (setq digit (digit-char-p char)))
                              ,expr
                              (return))))))
      (accumulate (setq number (+ (* number 10) digit)))
    ;; Deal with the dot, if it's there.
      (when (char= char #\.)
      ;; Read digits after the dot.
        (accumulate (setq divisor (* divisor 10)
                          number (+ (* number 10) digit))))
    ;; Is there an exponent letter?
      (cond
          ((null char)
           ;; If not, we've read the whole number.
           (let ((num (make-float-aux number divisor
                                      *read-default-float-format*
                                      stream)))
             (return-from make-float (if negative-fraction (- num) num))))
          ((= (get-constituent-trait char) +char-attr-constituent-expt+)
           (setq float-char char)
           ;; Check leading sign.
           (!setq-optional-leading-sign negative-exponent buf nil)
           ;; Read digits for exponent.
           (accumulate (setq exponent (+ (* exponent 10) digit)))
           (setq exponent (if negative-exponent (- exponent) exponent))
           ;; Generate and return the float, depending on FLOAT-CHAR:
           (let* ((float-format (case (char-upcase float-char)
                                  (#\E *read-default-float-format*)
                                  (#\S 'short-float)
                                  (#\F 'single-float)
                                  (#\D 'double-float)
                                  (#\L 'long-float)))
                  (exponent (truncate-exponent exponent number divisor))
                  (result (make-float-aux (* (expt 10 exponent) number)
                                          divisor float-format stream)))
             (return-from make-float
               (if negative-fraction (- result) result))))
          (t (bug "bad fallthrough in floating point reader"))))))

(defun make-float-aux (number divisor float-format stream)
  (handler-case
      (coerce (/ number divisor) float-format)
    (type-error (c)
      (error 'reader-impossible-number-error
             :error c :stream stream
             :format-control "failed to build float from ~a"
             :format-arguments (list (copy-token-buf-string *read-buffer*))))))

(defun make-ratio (stream)
  ;; Assume *READ-BUFFER* contains a legal ratio. Build the number from
  ;; the string.
  ;; This code is inferior to that of MAKE-INTEGER because it makes no
  ;; attempt to perform as few bignum multiplies as possible.
  ;;
  (let ((numerator 0) (denominator 0) (negativep nil)
        (base *read-base*) (buf *read-buffer*))
    (!setq-optional-leading-sign negativep buf t)
    ;; Get numerator.
    (loop (let ((dig (digit-char-p (token-buf-getchar buf) base)))
            (if dig
                (setq numerator (+ (* numerator base) dig))
                (return))))
    ;; Get denominator.
    (do* ((ch (token-buf-getchar buf) (token-buf-getchar buf))
          (dig ()))
         ((or (null ch) (not (setq dig (digit-char-p ch base)))))
         (setq denominator (+ (* denominator base) dig)))
    (let ((num (handler-case
                   (/ numerator denominator)
                 (arithmetic-error (c)
                   (error 'reader-impossible-number-error
                          :error c :stream stream
                          :format-control "failed to build ratio")))))
      (if negativep (- num) num))))

;;;; General reader for dispatch macros

(defun dispatch-char-error (stream sub-char ignore)
  (declare (ignore ignore))
  (if *read-suppress*
      (values)
      (simple-reader-error stream
                           "no dispatch function defined for ~S"
                           sub-char)))

(defun read-dispatch-char (stream dispatch-table)
  ;; Read some digits.
  (let ((numargp nil)
        (numarg 0)
        (sub-char ()))
    (loop
     (let ((ch (read-char stream nil +EOF+)))
       (if (eq ch +EOF+)
           (reader-eof-error stream "inside dispatch character")
          ;; Take care of the extra char.
           (let ((dig (digit-char-p ch)))
             (if dig
                 (setq numargp t numarg (+ (* numarg 10) dig))
                 (return (setq sub-char (char-upcase ch))))))))
    ;; Look up the function and call it.
    (let ((fn (get-raw-cmt-dispatch-entry sub-char dispatch-table)))
      (funcall (!cmt-entry-to-function fn #'dispatch-char-error)
               stream sub-char (if numargp numarg nil)))))

;;;; READ-FROM-STRING

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

(locally
(declare (muffle-conditions style-warning))
(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace)
  #!+sb-doc
  "The characters of string are successively given to the lisp reader
   and the lisp object built by the reader is returned. Macro chars
   will take effect."
  (declare (string string))
  (maybe-note-read-from-string-signature-issue eof-error-p)
  (%read-from-string string eof-error-p eof-value start end preserve-whitespace)))

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
  (!cold-init-standard-readtable))

(defmethod print-object ((readtable readtable) stream)
  (print-unreadable-object (readtable stream :identity t :type t)))

;; Backward-compatibility adapter. The "named-readtables" system in
;; Quicklisp expects this interface, and it's a reasonable thing to support.
;; What is silly however is that DISPATCH-TABLES was an alist each of whose
;; values was a hashtable which got immediately coerced to an alist.
;; In anticipation of perhaps not doing an extra re-shaping, if HASH-TABLE-P
;; is NIL then return nested alists: ((#\# (#\R . #<FUNCTION SHARP-R>) ...))
(defun dispatch-tables (readtable &optional (hash-table-p t))
  (let (alist)
    (flet ((process (char fn &aux (dtable (%dispatch-macro-char-table fn)))
             (when dtable
               (let ((output (awhen (car dtable) (%hash-table-alist it))))
                 (loop for fn across (the simple-vector (cdr dtable))
                       and ch from 0
                       when fn do (push (cons (code-char ch) fn) output))
                 (dolist (cell output) ; coerce values to function-designator
                   (rplacd cell (!cmt-entry-to-fun-designator (cdr cell))))
                 (when hash-table-p ; caller wants hash-tables
                   (setq output (%stuff-hash-table (make-hash-table) output)))
                 (push (cons char output) alist)))))
      (loop for fn across (character-macro-array readtable) and ch from 0
            do (process (code-char ch) fn))
      (maphash #'process (character-macro-hash-table readtable)))
    alist))

;; Stub - should never get called with anything but NIL
;; and only after all macros have been changed to constituents already.
(defun (setf dispatch-tables) (new-alist readtable)
  (declare (ignore readtable))
  (unless (null new-alist)
    (error "Assignment to virtual DISPATCH-TABLES slot not allowed"))
  new-alist)

;;; like LISTEN, but any whitespace in the input stream will be flushed
(defun listen-skip-whitespace (&optional (stream *standard-input*))
  (do ((char (read-char-no-hang stream nil nil nil)
             (read-char-no-hang stream nil nil nil)))
      ((null char) nil)
    (cond ((not (whitespace[1]p char))
           (unread-char char stream)
           (return t)))))
