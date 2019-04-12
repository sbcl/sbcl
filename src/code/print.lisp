;;;; the printer

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; exported printer control variables

;; NB: all of the following are initialized during genesis
(defparameter *print-readably* nil
  "If true, all objects will be printed readably. If readable printing
  is impossible, an error will be signalled. This overrides the value of
  *PRINT-ESCAPE*.")
(defparameter *print-escape* t
  "Should we print in a reasonably machine-readable way? (possibly
  overridden by *PRINT-READABLY*)")
(defparameter *print-pretty* nil ; (set later when pretty-printer is initialized)
  "Should pretty printing be used?")
(defparameter *print-base* 10.
  "The output base for RATIONALs (including integers).")
(defparameter *print-radix* nil
  "Should base be verified when printing RATIONALs?")
(defparameter *print-level* nil
  "How many levels should be printed before abbreviating with \"#\"?")
(defparameter *print-length* nil
  "How many elements at any level should be printed before abbreviating
  with \"...\"?")
(defparameter *print-vector-length* nil
  "Like *PRINT-LENGTH* but works on strings and bit-vectors.
Does not affect the cases that are already controlled by *PRINT-LENGTH*")
(defparameter *print-circle* nil
  "Should we use #n= and #n# notation to preserve uniqueness in general (and
  circularity in particular) when printing?")
(defparameter *print-case* :upcase
  "What case should the printer should use default?")
(defparameter *print-array* t
  "Should the contents of arrays be printed?")
(defparameter *print-gensym* t
  "Should #: prefixes be used when printing symbols with null SYMBOL-PACKAGE?")
(defparameter *print-lines* nil
  "The maximum number of lines to print per object.")
(defparameter *print-right-margin* nil
  "The position of the right margin in ems (for pretty-printing).")
(defparameter *print-miser-width* nil
  "If the remaining space between the current column and the right margin
   is less than this, then print using ``miser-style'' output. Miser
   style conditional newlines are turned on, and all indentations are
   turned off. If NIL, never use miser mode.")
(defvar *print-pprint-dispatch*
  (sb-pretty::make-pprint-dispatch-table) ; for type-correctness
  "The pprint-dispatch-table that controls how to pretty-print objects.")
(defparameter *suppress-print-errors* nil
  "Suppress printer errors when the condition is of the type designated by this
variable: an unreadable object representing the error is printed instead.")

;; duplicate defglobal because this file is compiled before "reader"
(define-load-time-global *standard-readtable* nil)

(defun %with-standard-io-syntax (function)
  (declare (type function function))
  (declare (dynamic-extent function))
  (let ((*package* #.(find-package "COMMON-LISP-USER"))
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
        (*print-pprint-dispatch* sb-pretty::*standard-pprint-dispatch-table*)
        (*print-pretty* nil)
        (*print-radix* nil)
        (*print-readably* t)
        (*print-right-margin* nil)
        (*read-base* 10)
        (*read-default-float-format* 'single-float)
        (*read-eval* t)
        (*read-suppress* nil)
        (*readtable* *standard-readtable*)
        (*suppress-print-errors* nil)
        (*print-vector-length* nil))
    (funcall function)))

;;;; routines to print objects

(macrolet ((def (fn doc &rest forms)
             `(defun ,fn
                    (object
                     &key
                     ,@(if (eq fn 'write) '(stream))
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
                      *print-pprint-dispatch*)
                     ((:suppress-errors *suppress-print-errors*)
                      *suppress-print-errors*))
               ,doc
               (declare (explicit-check))
               ,@forms)))
  (def write
       "Output OBJECT to the specified stream, defaulting to *STANDARD-OUTPUT*."
       (output-object object (out-stream-from-designator stream))
       object)
  (def write-to-string
       "Return the printed representation of OBJECT as a string."
       (stringify-object object)))

;;; Same as a call to (WRITE OBJECT :STREAM STREAM), but returning OBJECT.
(defun %write (object stream)
  (declare (explicit-check))
  (output-object object (out-stream-from-designator stream))
  object)

(defun prin1 (object &optional stream)
  "Output a mostly READable printed representation of OBJECT on the specified
  STREAM."
  (declare (explicit-check))
  (let ((*print-escape* t))
    (output-object object (out-stream-from-designator stream)))
  object)

(defun princ (object &optional stream)
  "Output an aesthetic but not necessarily READable printed representation
  of OBJECT on the specified STREAM."
  (declare (explicit-check))
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (output-object object (out-stream-from-designator stream)))
  object)

(defun print (object &optional stream)
  "Output a newline, the mostly READable printed representation of OBJECT, and
  space to the specified STREAM."
  (declare (explicit-check))
  (let ((stream (out-stream-from-designator stream)))
    (terpri stream)
    (prin1 object stream)
    (write-char #\space stream)
    object))

(defun pprint (object &optional stream)
  "Prettily output OBJECT preceded by a newline."
  (declare (explicit-check))
  (let ((*print-pretty* t)
        (*print-escape* t)
        (stream (out-stream-from-designator stream)))
    (terpri stream)
    (output-object object stream))
  (values))

(defun prin1-to-string (object)
  "Return the printed representation of OBJECT as a string with
   slashification on."
  (let ((*print-escape* t))
    (stringify-object object)))

(defun princ-to-string (object)
  "Return the printed representation of OBJECT as a string with
  slashification off."
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (stringify-object object)))

;;; This produces the printed representation of an object as a string.
;;; The few ...-TO-STRING functions above call this.
(defun stringify-object (object)
  (typecase object
    (integer
     (multiple-value-bind (fun pretty)
         (and *print-pretty* (pprint-dispatch object))
       (if pretty
           (with-simple-output-to-string (stream)
              (sb-pretty::with-pretty-stream (stream)
                (funcall fun stream object)))
           (let ((buffer-size (approx-chars-in-repr object)))
             (let* ((string (make-string buffer-size :element-type 'base-char))
                    (stream (%make-finite-base-string-output-stream string)))
               (declare (inline %make-finite-base-string-output-stream))
               (declare (truly-dynamic-extent stream))
               (output-integer object stream *print-base* *print-radix*)
               (%shrink-vector string
                               (finite-base-string-output-stream-pointer stream)))))))
    ;; Could do something for other numeric types, symbols, ...
    (t
     (with-simple-output-to-string (stream)
       (output-object object stream)))))

;;; Estimate the number of chars in the printed representation of OBJECT.
;;; The answer must be an overestimate or exact; never an underestimate.
(defun approx-chars-in-repr (object)
  (declare (integer object))
  ;; Round *PRINT-BASE* down to the nearest lower power-of-2, call that N,
  ;; and "guess" that the one character can represent N bits.
  ;; This is exact for bases which are exactly a power-of-2, or an overestimate
  ;; otherwise, as mandated by the finite output stream.
  (let ((bits-per-char
         (aref #.(sb-xc:coerce
                  ;; base 2 or base 3  = 1 bit per character
                  ;; base 4 .. base 7  = 2 bits per character
                  ;; base 8 .. base 15 = 3 bits per character, etc
                  #(1 1 2 2 2 2 3 3 3 3 3 3 3 3
                    4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5)
                  '(vector (unsigned-byte 8)))
               (- *print-base* 2))))
    (+ (if (minusp object) 1 0) ; leading sign
       (if *print-radix* 4 0) ; #rNN or trailing decimal
       (ceiling (if (fixnump object)
                    sb-vm:n-positive-fixnum-bits
                    (* (%bignum-length object) sb-bignum::digit-size))
                bits-per-char))))

;;;; support for the PRINT-UNREADABLE-OBJECT macro

(defun print-not-readable-error (object stream)
  (restart-case
      (error 'print-not-readable :object object)
    (print-unreadably ()
      :report "Print unreadably."
      (let ((*print-readably* nil))
        (output-object object stream)
        object))
    (use-value (o)
      :report "Supply an object to be printed instead."
      :interactive
      (lambda ()
        (read-evaluated-form "~@<Enter an object (evaluated): ~@:>"))
      (output-object o stream)
      o)))

;;; guts of PRINT-UNREADABLE-OBJECT
(defun %print-unreadable-object (object stream flags &optional body)
  (declare (type (or null function) body))
  (if *print-readably*
      (print-not-readable-error object stream)
      (flet ((print-description (&aux (type (logbitp 0 (truly-the (mod 4) flags)))
                                      (identity (logbitp 1 flags)))
               (when type
                 (write (type-of object) :stream stream :circle nil
                                         :level nil :length nil)
                 ;; Do NOT insert a pprint-newline here.
                 ;; See ba34717602d80e5fd74d10e61f4729fb0d019a0c
                 (write-char #\space stream))
               (when body
                 (funcall body))
               (when identity
                 (when (or body (not type))
                   (write-char #\space stream))
                 ;; Nor here.
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
               (write-char #\> stream)))))
  nil)

;;;; OUTPUT-OBJECT -- the main entry point

;;; Objects whose print representation identifies them EQLly don't
;;; need to be checked for circularity.
(defun uniquely-identified-by-print-p (x)
  (or (numberp x)
      (characterp x)
      (and (symbolp x)
           (sb-xc:symbol-package x))))

(defvar *in-print-error* nil)

;;; Output OBJECT to STREAM observing all printer control variables.
(defun output-object (object stream)
  ;; FIXME: this function is declared EXPLICIT-CHECK, so it allows STREAM
  ;; to be T or NIL (a stream-designator), which is not really right
  ;; if eventually the call will be to a PRINT-OBJECT method,
  ;; since the generic function should always receive a stream.
  (declare (explicit-check))
  (labels ((print-it (stream)
             (multiple-value-bind (fun pretty)
                 (and *print-pretty* (pprint-dispatch object))
               (if pretty
                   (sb-pretty::with-pretty-stream (stream)
                     (funcall fun stream object))
                   (output-ugly-object stream object))))
           (handle-it (stream)
             (if *suppress-print-errors*
                 (handler-bind
                     ((condition
                       (lambda (condition)
                         (when (typep condition *suppress-print-errors*)
                           (cond (*in-print-error*
                                  (write-string "(error printing " stream)
                                  (write-string *in-print-error* stream)
                                  (write-string ")" stream))
                                 (t
                                  (let ((*print-readably* nil)
                                        (*print-escape* t))
                                    (write-string
                                     "#<error printing a " stream)
                                    (let ((*in-print-error* "type"))
                                      (output-object (type-of object) stream))
                                    (write-string ": " stream)
                                    (let ((*in-print-error* "condition"))
                                      (output-object condition stream))
                                    (write-string ">" stream))))
                           (return-from handle-it object)))))
                   (print-it stream))
                 (print-it stream)))
           (check-it (stream)
             (multiple-value-bind (marker initiate)
                 (check-for-circularity object t)
               (if (eq initiate :initiate)
                   (let ((*circularity-hash-table*
                          (make-hash-table :test 'eq)))
                     (check-it (make-broadcast-stream))
                     (let ((*circularity-counter* 0))
                       (check-it stream)))
                   ;; otherwise
                   (if marker
                       (when (handle-circularity marker stream)
                         (handle-it stream))
                       (handle-it stream))))))
    (cond (;; Maybe we don't need to bother with circularity detection.
           (or (not *print-circle*)
               (uniquely-identified-by-print-p object))
           (handle-it stream))
          (;; If we have already started circularity detection, this
           ;; object might be a shared reference. If we have not, then
           ;; if it is a compound object it might contain a circular
           ;; reference to itself or multiple shared references.
           (or *circularity-hash-table*
               (compound-object-p object))
           (check-it stream))
          (t
           (handle-it stream)))))

;;; Output OBJECT to STREAM observing all printer control variables
;;; except for *PRINT-PRETTY*. Note: if *PRINT-PRETTY* is non-NIL,
;;; then the pretty printer will be used for any components of OBJECT,
;;; just not for OBJECT itself.
(defun output-ugly-object (stream object)
  (when (%instancep object)
    (let* ((layout (layout-of object))
           (classoid (layout-classoid layout)))
      ;; If an instance has no layout, it has no PRINT-OBJECT method.
      ;; Additionally, if the object is an obsolete CONDITION, don't crash.
      ;; (There is no update-instance protocol for conditions)
      (when (or (sb-kernel::undefined-classoid-p classoid)
                (and (layout-invalid layout)
                     (logtest (layout-%bits layout) +condition-layout-flag+)))
        ;; not only is this unreadable, it's unprintable too.
        (return-from output-ugly-object
          (print-unreadable-object (object stream :identity t)
            (format stream "UNPRINTABLE instance of ~W" classoid))))))
  (print-object object stream))

;;;; symbols

(defmethod print-object ((object symbol) stream)
  (if (or *print-escape* *print-readably*)
      ;; Write so that reading back works
      (output-symbol object (sb-xc:symbol-package object) stream)
      ;; Write only the characters of the name, never the package
      (let ((rt *readtable*))
        (funcall (truly-the function
                  (choose-symbol-out-fun *print-case* (%readtable-case rt)))
                 (symbol-name object) stream rt))))

(defun output-symbol (symbol package stream)
  (let* ((readably *print-readably*)
         (readtable (if readably *standard-readtable* *readtable*))
         (out-fun (choose-symbol-out-fun *print-case* (%readtable-case readtable))))
    (flet ((output-token (name)
             (declare (type simple-string name))
             (cond ((or (and (readtable-normalization readtable)
                             (not (sb-unicode:normalized-p name :nfkc)))
                        (symbol-quotep name readtable))
                    ;; Output NAME surrounded with |'s,
                    ;; and with any embedded |'s or \'s escaped.
                    (write-char #\| stream)
                    (dotimes (index (length name))
                      (let ((char (char name index)))
                        ;; Hmm. Should these depend on what characters
                        ;; are actually escapes in the readtable ?
                        ;; (See similar remark at DEFUN QUOTE-STRING)
                        (when (or (char= char #\\) (char= char #\|))
                          (write-char #\\ stream))
                        (write-char char stream)))
                    (write-char #\| stream))
                   (t
                    (funcall (truly-the function out-fun) name stream readtable)))))
      (let ((name (symbol-name symbol))
            (current (sane-package)))
        (cond
         ;; The ANSI spec "22.1.3.3.1 Package Prefixes for Symbols"
         ;; requires that keywords be printed with preceding colons
         ;; always, regardless of the value of *PACKAGE*.
         ((eq package *keyword-package*)
          (write-char #\: stream))
         ;; Otherwise, if the symbol's home package is the current
         ;; one, then a prefix is never necessary.
         ((eq package current))
         ;; Uninterned symbols print with a leading #:.
         ((null package)
          (when (or *print-gensym* readably)
            (write-string "#:" stream)))
         (t
          (multiple-value-bind (found accessible) (find-symbol name current)
            ;; If we can find the symbol by looking it up, it need not
            ;; be qualified. This can happen if the symbol has been
            ;; inherited from a package other than its home package.
            ;;
            ;; To preserve print-read consistency, use the local nickname if
            ;; one exists.
            (unless (and accessible (eq found symbol))
              (output-token (or (package-local-nickname package current)
                                (package-name package)))
              (write-string (if (eql (find-external-symbol name package) 0) "::" ":")
                            stream)))))
        (output-token name)))))

;;;; escaping symbols

;;; When we print symbols we have to figure out if they need to be
;;; printed with escape characters. This isn't a whole lot easier than
;;; reading symbols in the first place.
;;;
;;; For each character, the value of the corresponding element is a
;;; fixnum with bits set corresponding to attributes that the
;;; character has. All characters have at least one bit set, so we can
;;; search for any character with a positive test.

;;; constants which are a bit-mask for each interesting character attribute
(defconstant other-attribute            (ash 1 0)) ; Anything else legal.
(defconstant number-attribute           (ash 1 1)) ; A numeric digit.
(defconstant uppercase-attribute        (ash 1 2)) ; An uppercase letter.
(defconstant lowercase-attribute        (ash 1 3)) ; A lowercase letter.
(defconstant sign-attribute             (ash 1 4)) ; +-
(defconstant extension-attribute        (ash 1 5)) ; ^_
(defconstant dot-attribute              (ash 1 6)) ; .
(defconstant slash-attribute            (ash 1 7)) ; /
(defconstant funny-attribute            (ash 1 8)) ; Anything illegal.

;;; LETTER-ATTRIBUTE is a local of SYMBOL-QUOTEP. It matches letters
;;; that don't need to be escaped (according to READTABLE-CASE.)
(defconstant-eqx +attribute-names+
  '((number . number-attribute) (lowercase . lowercase-attribute)
    (uppercase . uppercase-attribute) (letter . letter-attribute)
    (sign . sign-attribute) (extension . extension-attribute)
    (dot . dot-attribute) (slash . slash-attribute)
    (other . other-attribute) (funny . funny-attribute))
  #'equal)

;;; For each character, the value of the corresponding element is the
;;; lowest base in which that character is a digit.
(defconstant +digit-bases+
  #.(let ((a (sb-xc:make-array 128 ; FIXME
                               :element-type '(unsigned-byte 8)
                               :initial-element 36)))
      (dotimes (i 36 a)
        (let ((char (digit-char i 36)))
          (setf (aref a (sb-xc:char-code char)) i)))))

(defconstant +character-attributes+
  #.(let ((a (sb-xc:make-array 160 ; FIXME
                               :element-type '(unsigned-byte 16)
                               :initial-element 0)))
      (flet ((set-bit (char bit)
               (let ((code (sb-xc:char-code char)))
                 (setf (aref a code) (logior bit (aref a code))))))

        (dolist (char '(#\! #\@ #\$ #\% #\& #\* #\= #\~ #\[ #\] #\{ #\}
                        #\? #\< #\>))
          (set-bit char other-attribute))

        (dotimes (i 10)
          (set-bit (digit-char i) number-attribute))

        (do ((code (sb-xc:char-code #\A) (1+ code))
             (end (sb-xc:char-code #\Z)))
            ((> code end))
          (declare (fixnum code end))
          (set-bit (sb-xc:code-char code) uppercase-attribute)
          (set-bit (char-downcase (sb-xc:code-char code)) lowercase-attribute))

        (set-bit #\- sign-attribute)
        (set-bit #\+ sign-attribute)
        (set-bit #\^ extension-attribute)
        (set-bit #\_ extension-attribute)
        (set-bit #\. dot-attribute)
        (set-bit #\/ slash-attribute)

        ;; Mark anything not explicitly allowed as funny.
        (dotimes (i 160) ; FIXME
          (when (zerop (aref a i))
            (setf (aref a i) funny-attribute))))
      a))

;;; A FSM-like thingie that determines whether a symbol is a potential
;;; number or has evil characters in it.
(defun symbol-quotep (name readtable)
  (declare (simple-string name))
  (macrolet ((advance (tag &optional (at-end t))
               `(progn
                 (when (= index len)
                   ,(if at-end '(go TEST-SIGN) '(return nil)))
                 (setq current (schar name index)
                       code (char-code current)
                       bits (cond ; FIXME
                              ((< code 160) (aref attributes code))
                              ((upper-case-p current) uppercase-attribute)
                              ((lower-case-p current) lowercase-attribute)
                              (t other-attribute)))
                 (incf index)
                 (go ,tag)))
             (test (&rest attributes)
                `(not (zerop
                       (the fixnum
                            (logand
                             (logior ,@(mapcar
                                        (lambda (x)
                                          (or (cdr (assoc x
                                                          +attribute-names+))
                                              (error "Blast!")))
                                        attributes))
                             bits)))))
             (digitp ()
               `(and (< code 128) ; FIXME
                     (< (the fixnum (aref bases code)) base))))

    (prog ((len (length name))
           (attributes +character-attributes+)
           (bases +digit-bases+)
           (base *print-base*)
           (letter-attribute
            (case (%readtable-case readtable)
              (#.+readtable-upcase+ uppercase-attribute)
              (#.+readtable-downcase+ lowercase-attribute)
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
          (unless (zerop (logand (let* ((char (schar name i))
                                        (code (char-code char)))
                                   (cond
                                     ((< code 160) (aref attributes code))
                                     ((upper-case-p char) uppercase-attribute)
                                     ((lower-case-p char) lowercase-attribute)
                                     (t other-attribute)))
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
      ;; ("What," you may ask, "is a 'number marker'?" It's something
      ;; that a conforming implementation might use in number syntax.
      ;; See ANSI 2.3.1.1 "Potential Numbers as Tokens".)
      (when (test letter) (advance OTHER nil))
      (go DIGIT))))

;;;; case hackery: One of these functions is chosen to output symbol
;;;; names according to the values of *PRINT-CASE* and READTABLE-CASE.

;;; called when:
;;; READTABLE-CASE      *PRINT-CASE*
;;; :UPCASE             :UPCASE
;;; :DOWNCASE           :DOWNCASE
;;; :PRESERVE           any
(defun output-preserve-symbol (pname stream readtable)
  (declare (ignore readtable))
  (write-string pname stream))

;;; called when:
;;; READTABLE-CASE      *PRINT-CASE*
;;; :UPCASE             :DOWNCASE
(defun output-lowercase-symbol (pname stream readtable)
  (declare (simple-string pname) (ignore readtable))
  (dotimes (index (length pname))
    (let ((char (schar pname index)))
      (write-char (char-downcase char) stream))))

;;; called when:
;;; READTABLE-CASE      *PRINT-CASE*
;;; :DOWNCASE           :UPCASE
(defun output-uppercase-symbol (pname stream readtable)
  (declare (simple-string pname) (ignore readtable))
  (dotimes (index (length pname))
    (let ((char (schar pname index)))
      (write-char (char-upcase char) stream))))

;;; called when:
;;; READTABLE-CASE      *PRINT-CASE*
;;; :UPCASE             :CAPITALIZE
;;; :DOWNCASE           :CAPITALIZE
(defun output-capitalize-symbol (pname stream readtable)
  (declare (simple-string pname))
  (let ((prev-not-alphanum t)
        (up (eql (%readtable-case readtable) +readtable-upcase+)))
    (dotimes (i (length pname))
      (let ((char (char pname i)))
        (write-char (if up
                        (if (or prev-not-alphanum (lower-case-p char))
                            char
                            (char-downcase char))
                        (if prev-not-alphanum
                            (char-upcase char)
                            char))
                    stream)
        (setq prev-not-alphanum (not (alphanumericp char)))))))

;;; called when:
;;; READTABLE-CASE      *PRINT-CASE*
;;; :INVERT             any
(defun output-invert-symbol (pname stream readtable)
  (declare (simple-string pname) (ignore readtable))
  (let ((all-upper t)
        (all-lower t))
    (dotimes (i (length pname))
      (let ((ch (schar pname i)))
        (when (both-case-p ch)
          (if (upper-case-p ch)
              (setq all-lower nil)
              (setq all-upper nil)))))
    (cond (all-upper (output-lowercase-symbol pname stream nil))
          (all-lower (output-uppercase-symbol pname stream nil))
          (t
           (write-string pname stream)))))

(defun choose-symbol-out-fun (print-case readtable-case)
  (macrolet
      ((compute-fun-vector (&aux (vector (make-array 12)))
         ;; Pack a 2D array of functions into a simple-vector.
         ;; Major axis is *PRINT-CASE*, minor axis is %READTABLE-CASE.
         (dotimes (readtable-case-index 4)
           (dotimes (print-case-index 3)
             (let ((readtable-case
                    (elt '(:upcase :downcase :preserve :invert) readtable-case-index))
                   (print-case
                    (elt '(:upcase :downcase :capitalize) print-case-index)))
               (setf (aref vector (logior (ash print-case-index 2)
                                          readtable-case-index))
                     (case readtable-case
                       (:upcase
                        (case print-case
                          (:upcase 'output-preserve-symbol)
                          (:downcase 'output-lowercase-symbol)
                          (:capitalize 'output-capitalize-symbol)))
                       (:downcase
                        (case print-case
                          (:upcase 'output-uppercase-symbol)
                          (:downcase 'output-preserve-symbol)
                          (:capitalize 'output-capitalize-symbol)))
                       (:preserve 'output-preserve-symbol)
                       (:invert 'output-invert-symbol))))))
         `(load-time-value (vector ,@(map 'list (lambda (x) `(function ,x)) vector))
                           t)))
    (aref (compute-fun-vector)
          (logior (case print-case (:upcase 0) (:downcase 4) (t 8))
                  (truly-the (mod 4) readtable-case)))))

;;;; recursive objects

(defmethod print-object ((list cons) stream)
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

(defmethod print-object ((vector vector) stream)
  (let ((readably *print-readably*))
    (flet ((cut-length ()
             (when (and (not readably)
                        *print-vector-length*
                        (> (length vector) *print-vector-length*))
               (print-unreadable-object (vector stream :type t :identity t)
                 (format stream "~A..."
                         (make-array *print-vector-length*
                                     :element-type (array-element-type vector)
                                     :displaced-to vector)))
               t)))
      (cond ((stringp vector)
             (cond ((and readably (not (typep vector '(vector character))))
                    (output-unreadable-array-readably vector stream))
                   ((and *print-escape*
                         (cut-length)))
                   ((or *print-escape* readably)
                    (write-char #\" stream)
                    (quote-string vector stream)
                    (write-char #\" stream))
                   (t
                    (write-string vector stream))))
            ((not (or *print-array* readably))
             (output-terse-array vector stream))
            ((bit-vector-p vector)
             (cond ((cut-length))
                   (t
                    (write-string "#*" stream)
                    (dovector (bit vector)
                      ;; (Don't use OUTPUT-OBJECT here, since this code
                      ;; has to work for all possible *PRINT-BASE* values.)
                      (write-char (if (zerop bit) #\0 #\1) stream)))))
            ((or (not readably) (array-readably-printable-p vector))
             (descend-into (stream)
               (write-string "#(" stream)
               (dotimes (i (length vector))
                 (unless (zerop i)
                   (write-char #\space stream))
                 (punt-print-if-too-long i stream)
                 (output-object (aref vector i) stream))
               (write-string ")" stream)))

            (t
             (output-unreadable-array-readably vector stream))))))

;;; This function outputs a string quoting characters sufficiently
;;; so that someone can read it in again. Basically, put a slash in
;;; front of an character satisfying NEEDS-SLASH-P.
(defun quote-string (string stream)
  (macrolet ((needs-slash-p (char)
               ;; KLUDGE: We probably should look at the readtable, but just do
               ;; this for now. [noted by anonymous long ago] -- WHN 19991130
               `(or (char= ,char #\\)
                 (char= ,char #\"))))
    (with-array-data ((data string) (start) (end)
                      :check-fill-pointer t)
      (do ((index start (1+ index)))
          ((>= index end))
        (let ((char (schar data index)))
          (when (needs-slash-p char) (write-char #\\ stream))
          (write-char char stream))))))

(defun array-readably-printable-p (array)
  (and (eq (array-element-type array) t)
       (let ((zero (position 0 (array-dimensions array)))
             (number (position 0 (array-dimensions array)
                               :test (complement #'eql)
                               :from-end t)))
         (or (null zero) (null number) (> zero number)))))

;;; Output the printed representation of any array in either the #< or #A
;;; form.
(defmethod print-object ((array array) stream)
  (if (or *print-array* *print-readably*)
      (output-array-guts array stream)
      (output-terse-array array stream)))

;;; Output the abbreviated #< form of an array.
(defun output-terse-array (array stream)
  (let ((*print-level* nil)
        (*print-length* nil))
    (print-unreadable-object (array stream :type t :identity t))))

;;; Convert an array into a list that can be used with MAKE-ARRAY's
;;; :INITIAL-CONTENTS keyword argument.
(defun listify-array (array)
  (flet ((compact (seq)
           (typecase array
             (string
              (coerce seq '(simple-array character (*))))
             ((array bit)
              (coerce seq 'bit-vector))
             (t
              seq))))
    (if (typep array '(or string bit-vector))
        (compact array)
        (with-array-data ((data array) (start) (end))
          (declare (ignore end))
          (labels ((listify (dimensions index)
                     (if (null dimensions)
                         (aref data index)
                         (let* ((dimension (car dimensions))
                                (dimensions (cdr dimensions))
                                (count (reduce #'* dimensions)))
                           (loop for i below dimension
                                 for list = (listify dimensions index)
                                 collect (if (and dimensions
                                                  (null (cdr dimensions)))
                                             (compact list)
                                             list)
                                 do (incf index count))))))
            (listify (array-dimensions array) start))))))

;;; Use nonstandard #A(dimensions element-type contents)
;;; to avoid using #.
(defun output-unreadable-array-readably (array stream)
  (let ((array (list* (array-dimensions array)
                      (array-element-type array)
                      (listify-array array))))
    (write-string "#A" stream)
    (write array :stream stream)
    nil))

;;; Output the readable #A form of an array.
(defun output-array-guts (array stream)
  (cond ((or (not *print-readably*)
             (array-readably-printable-p array))
         (write-char #\# stream)
         (output-integer (array-rank array) stream 10 nil)
         (write-char #\A stream)
         (with-array-data ((data array) (start) (end))
           (declare (ignore end))
           (sub-output-array-guts data (array-dimensions array) stream start)))
        (t
         (output-unreadable-array-readably array stream))))

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


;;;; integer, ratio, and complex printing (i.e. everything but floats)

(defun %output-radix (base stream)
  (write-char #\# stream)
  (write-char (case base
                (2 #\b)
                (8 #\o)
                (16 #\x)
                (t (%output-reasonable-integer-in-base base 10 stream)
                   #\r))
              stream))

(defun %output-reasonable-integer-in-base (n base stream)
  (multiple-value-bind (q r)
      (truncate n base)
    ;; Recurse until you have all the digits pushed on
    ;; the stack.
    (unless (zerop q)
      (%output-reasonable-integer-in-base q base stream))
    ;; Then as each recursive call unwinds, turn the
    ;; digit (in remainder) into a character and output
    ;; the character.
    (write-char
     (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" r)
     stream)))

;;; *POWER-CACHE* is an alist mapping bases to power-vectors. It is
;;; filled and probed by POWERS-FOR-BASE. SCRUB-POWER-CACHE is called
;;; always prior a GC to drop overly large bignums from the cache.
;;;
;;; It doesn't need a lock, but if you work on SCRUB-POWER-CACHE or
;;; POWERS-FOR-BASE, see that you don't break the assumptions!
(define-load-time-global *power-cache* (make-array 37 :initial-element nil))
(declaim (type (simple-vector 37) *power-cache*))

(defconstant +power-cache-integer-length-limit+ 2048)

(defun scrub-power-cache (&aux (cache *power-cache*))
  (dotimes (i (length cache))
    (let ((powers (aref cache i)))
      (when powers
        (let ((too-big (position-if
                        (lambda (x)
                          (>= (integer-length x)
                              +power-cache-integer-length-limit+))
                        (the simple-vector powers))))
          (when too-big
            (setf (aref cache i) (subseq powers 0 too-big))))))))

;;; Compute (and cache) a power vector for a BASE and LIMIT:
;;; the vector holds integers for which
;;;    (aref powers k) == (expt base (expt 2 k))
;;; holds.
(defun powers-for-base (base limit)
  (flet ((compute-powers (from)
           (let (powers)
             (do ((p from (* p p)))
                 ((> p limit)
                  ;; We don't actually need this, but we also
                  ;; prefer not to cons it up a second time...
                  (push p powers))
               (push p powers))
             (nreverse powers))))
    (let* ((cache *power-cache*)
           (powers (aref cache base)))
      (setf (aref cache base)
            (concatenate 'vector powers
                         (compute-powers
                          (if powers
                              (let* ((len (length powers))
                                     (max (svref powers (1- len))))
                                (if (> max limit)
                                    (return-from powers-for-base powers)
                                    (* max max)))
                              base)))))))

;; Algorithm by Harald Hanche-Olsen, sbcl-devel 2005-02-05
(defun %output-huge-integer-in-base (n base stream)
  (declare (type bignum n) (type fixnum base))
  ;; POWER is a vector for which the following holds:
  ;;   (aref power k) == (expt base (expt 2 k))
  (let* ((power (powers-for-base base n))
         (k-start (or (position-if (lambda (x) (> x n)) power)
                      (bug "power-vector too short"))))
    (labels ((bisect (n k exactp)
               (declare (fixnum k))
               ;; N is the number to bisect
               ;; K on initial entry BASE^(2^K) > N
               ;; EXACTP is true if 2^K is the exact number of digits
               (cond ((zerop n)
                      (when exactp
                        (loop repeat (ash 1 k) do (write-char #\0 stream))))
                     ((zerop k)
                      (write-char
                       (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" n)
                       stream))
                     (t
                      (setf k (1- k))
                      (multiple-value-bind (q r) (truncate n (aref power k))
                        ;; EXACTP is NIL only at the head of the
                        ;; initial number, as we don't know the number
                        ;; of digits there, but we do know that it
                        ;; doesn't get any leading zeros.
                        (bisect q k exactp)
                        (bisect r k (or exactp (plusp q))))))))
      (bisect n k-start nil))))

(defun %output-integer-in-base (integer base stream)
  (when (minusp integer)
    (write-char #\- stream)
    (setf integer (- integer)))
  ;; The ideal cutoff point between these two algorithms is almost
  ;; certainly quite platform dependent: this gives 87 for 32 bit
  ;; SBCL, which is about right at least for x86/Darwin.
  (if (or (fixnump integer)
          (< (integer-length integer) (* 3 sb-vm:n-positive-fixnum-bits)))
      (%output-reasonable-integer-in-base integer base stream)
      (%output-huge-integer-in-base integer base stream)))

;;; This gets both a method and a specifically named function
;;; since the latter is called from a few places.
(defmethod print-object ((object integer) stream)
  (output-integer object stream *print-base* *print-radix*))
(defun output-integer (integer stream base radixp)
  (cond (radixp
         (unless (= base 10) (%output-radix base stream))
         (%output-integer-in-base integer base stream)
         (when (= base 10) (write-char #\. stream)))
        (t
         (%output-integer-in-base integer base stream))))

(defmethod print-object ((ratio ratio) stream)
  (let ((base *print-base*))
    (when *print-radix*
      (%output-radix base stream))
    (%output-integer-in-base (numerator ratio) base stream)
    (write-char #\/ stream)
    (%output-integer-in-base (denominator ratio) base stream)))

(defmethod print-object ((complex complex) stream)
  (write-string "#C(" stream)
  (output-object (realpart complex) stream)
  (write-char #\space stream)
  (output-object (imagpart complex) stream)
  (write-char #\) stream))

;;;; float printing

;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does
;;; most of the work for all printing of floating point numbers in
;;; FORMAT.  It converts a floating point number to a string in a free
;;; or fixed format with no exponent. The interpretation of the
;;; arguments is as follows:
;;;
;;;     X       - The floating point number to convert, which must not be
;;;             negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;             of fraction digits to produce if the FDIGITS parameter
;;;             is unspecified or NIL. If the non-fraction digits and the
;;;             decimal point alone exceed this width, no fraction digits
;;;             will be produced unless a non-NIL value of FDIGITS has been
;;;             specified. Field overflow is not considerd an error at this
;;;             level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;             trailing zeroes may be introduced as needed. May be
;;;             unspecified or NIL, in which case as many digits as possible
;;;             are generated, subject to the constraint that there are no
;;;             trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;             printed is (* x (expt 10 scale)). This scaling is exact,
;;;             and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;             number of fraction digits which will be produced, regardless
;;;             of the value of WIDTH or FDIGITS. This feature is used by
;;;             the ~E format directive to prevent complete loss of
;;;             significance in the printed value due to a bogus choice of
;;;             scale factor.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                    decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                    decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                    point. Zero indicates point before first digit.
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
;;; FLOAT-DIGITS actually generates the digits for positive numbers;
;;; see below for comments.

(defun flonum-to-string (x &optional width fdigits scale fmin)
  (declare (type float x))
  (multiple-value-bind (e string)
      (if fdigits
          (flonum-to-digits x (min (- (+ fdigits (or scale 0)))
                                   (- (or fmin 0))))
          (if (and width (> width 1))
              (let ((w (multiple-value-list
                        (flonum-to-digits x
                                          (max 1
                                               (+ (1- width)
                                                  (if (and scale (minusp scale))
                                                      scale 0)))
                                          t)))
                    (f (multiple-value-list
                        (flonum-to-digits x (- (+ (or fmin 0)
                                                  (if scale scale 0)))))))
                (cond
                  ((>= (length (cadr w)) (length (cadr f)))
                   (values-list w))
                  (t (values-list f))))
              (flonum-to-digits x)))
    (let ((e (if (zerop x)
                 e
                 (+ e (or scale 0))))
          (stream (make-string-output-stream)))
      (if (plusp e)
          (progn
            (write-string string stream :end (min (length string) e))
            (dotimes (i (- e (length string)))
              (write-char #\0 stream))
            (write-char #\. stream)
            (write-string string stream :start (min (length string) e))
            (when fdigits
              (dotimes (i (- fdigits
                             (- (length string)
                                (min (length string) e))))
                (write-char #\0 stream))))
          (progn
            (write-string "." stream)
            (dotimes (i (- e))
              (write-char #\0 stream))
            (write-string string stream :end (when fdigits
                                               (min (length string)
                                                    (max (or fmin 0)
                                                         (+ fdigits e)))))
            (when fdigits
              (dotimes (i (+ fdigits e (- (length string))))
                (write-char #\0 stream)))))
      (let ((string (get-output-stream-string stream)))
        (values string (length string)
                (char= (char string 0) #\.)
                (char= (char string (1- (length string))) #\.)
                (position #\. string))))))

;;; implementation of figure 1 from Burger and Dybvig, 1996. It is
;;; extended in order to handle rounding.
;;;
;;; As the implementation of the Dragon from Classic CMUCL (and
;;; previously in SBCL above FLONUM-TO-STRING) says: "DO NOT EVEN
;;; THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING THE
;;; PAPER!", and in this case we have to add that even reading the
;;; paper might not bring immediate illumination as CSR has attempted
;;; to turn idiomatic Scheme into idiomatic Lisp.
;;;
;;; FIXME: figure 1 from Burger and Dybvig is the unoptimized
;;; algorithm, noticeably slow at finding the exponent.  Figure 2 has
;;; an improved algorithm, but CSR ran out of energy.
;;;
;;; possible extension for the enthusiastic: printing floats in bases
;;; other than base 10.
(defconstant single-float-min-e
  (- 2 sb-vm:single-float-bias sb-vm:single-float-digits))
(defconstant double-float-min-e
  (- 2 sb-vm:double-float-bias sb-vm:double-float-digits))
#+long-float
(defconstant long-float-min-e
  (nth-value 1 (decode-float least-positive-long-float)))

;;; Call CHAR-FUN with the digits of FLOAT
;;; PROLOGUE-FUN and EPILOGUE-FUN are called with the exponent before
;;; and after printing to set up the state.
(declaim (inline %flonum-to-digits))
(defun %flonum-to-digits (char-fun
                          prologue-fun
                          epilogue-fun
                          float &optional position relativep)
  (let ((print-base 10)                 ; B
        (float-radix 2)                 ; b
        (float-digits (float-digits float)) ; p
        (min-e
          (etypecase float
            (single-float single-float-min-e)
            (double-float double-float-min-e)
            #+long-float
            (long-float long-float-min-e))))
    (multiple-value-bind (f e)
        (integer-decode-float float)
      (let ( ;; FIXME: these even tests assume normal IEEE rounding
            ;; mode.  I wonder if we should cater for non-normal?
            (high-ok (evenp f))
            (low-ok (evenp f)))
        (labels ((scale (r s m+ m-)
                   (do ((r+m+ (+ r m+))
                        (k 0 (1+ k))
                        (s s (* s print-base)))
                       ((not (or (> r+m+ s)
                                 (and high-ok (= r+m+ s))))
                        (do ((k k (1- k))
                             (r r (* r print-base))
                             (m+ m+ (* m+ print-base))
                             (m- m- (* m- print-base)))
                            ((not (and (> r m-) ; Extension to handle zero
                                       (let ((x (* (+ r m+) print-base)))
                                         (or (< x s)
                                             (and (not high-ok)
                                                  (= x s))))))
                             (funcall prologue-fun k)
                             (generate r s m+ m-)
                             (funcall epilogue-fun k))))))
                 (generate (r s m+ m-)
                   (let (d tc1 tc2)
                     (tagbody
                      loop
                        (setf (values d r) (truncate (* r print-base) s))
                        (setf m+ (* m+ print-base))
                        (setf m- (* m- print-base))
                        (setf tc1 (or (< r m-) (and low-ok (= r m-))))
                        (setf tc2 (let ((r+m+ (+ r m+)))
                                    (or (> r+m+ s)
                                        (and high-ok (= r+m+ s)))))
                        (when (or tc1 tc2)
                          (go end))
                        (funcall char-fun d)
                        (go loop)
                      end
                        (let ((d (cond
                                   ((and (not tc1) tc2) (1+ d))
                                   ((and tc1 (not tc2)) d)
                                   ((< (* r 2) s)
                                    d)
                                   (t
                                    (1+ d)))))
                          (funcall char-fun d)))))
                 (initialize ()
                   (let (r s m+ m-)
                     (cond ((>= e 0)
                            (let ((be (expt float-radix e)))
                              (if (/= f (expt float-radix (1- float-digits)))
                                  ;; multiply F by 2 first, avoding consing two bignums
                                  (setf r (* f 2 be)
                                        s 2
                                        m+ be
                                        m- be)
                                  (setf m- be
                                        m+ (* be float-radix)
                                        r (* f 2 m+)
                                        s (* float-radix 2)))))
                           ((or (= e min-e)
                                (/= f (expt float-radix (1- float-digits))))
                            (setf r (* f 2)
                                  s (expt float-radix (- 1 e))
                                  m+ 1
                                  m- 1))
                           (t
                            (setf r (* f float-radix 2)
                                  s (expt float-radix (- 2 e))
                                  m+ float-radix
                                  m- 1)))
                     (when position
                       (when relativep
                         (aver (> position 0))
                         (do ((k 0 (1+ k))
                              ;; running out of letters here
                              (l 1 (* l print-base)))
                             ((>= (* s l) (+ r m+))
                              ;; k is now \hat{k}
                              (if (< (+ r (* s (/ (expt print-base (- k position)) 2)))
                                     (* s l))
                                  (setf position (- k position))
                                  (setf position (- k position 1))))))
                       (let* ((x (/ (* s (expt print-base position)) 2))
                              (low (max m- x))
                              (high (max m+ x)))
                         (when (<= m- low)
                           (setf m- low)
                           (setf low-ok t))
                         (when (<= m+ high)
                           (setf m+ high)
                           (setf high-ok t))))
                     (values r s m+ m-))))
          (multiple-value-bind (r s m+ m-) (initialize)
            (scale r s m+ m-)))))))

(defun flonum-to-digits (float &optional position relativep)
  (let ((digit-characters "0123456789"))
    (with-push-char (:element-type base-char)
      (%flonum-to-digits
       (lambda (d)
         (push-char (char digit-characters d)))
       (lambda (k) k)
       (lambda (k) (values k (get-pushed-string)))
       float position relativep))))

(defun print-float (float stream)
  (let ((position 0)
        (dot-position 0)
        (digit-characters "0123456789")
        (e-min -3)
        (e-max 8))
    (%flonum-to-digits
     (lambda (d)
       (when (= position dot-position)
         (write-char #\. stream))
       (write-char (char digit-characters d) stream)
       (incf position))
     (lambda (k)
       (cond ((not (< e-min k e-max))
              (setf dot-position 1))
             ((plusp k)
              (setf dot-position k))
             (t
              (setf dot-position -1)
              (write-char #\0 stream)
              (write-char #\. stream)
              (loop for i below (- k)
                    do (write-char #\0 stream)))))
     (lambda (k)
       (when (<= position dot-position)
         (loop for i below (- dot-position position)
               do (write-char #\0 stream))
         (write-char #\. stream)
         (write-char #\0 stream))
       (if (< e-min k e-max)
           (print-float-exponent float 0 stream)
           (print-float-exponent float (1- k) stream)))
     float)))

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

(eval-when (:compile-toplevel :execute)
  (setf *read-default-float-format*
        #+long-float 'cl:long-float #-long-float 'cl:double-float))
(defun scale-exponent (original-x)
  (let* ((x (coerce original-x 'long-float)))
    (multiple-value-bind (sig exponent) (decode-float x)
      (declare (ignore sig))
      (if (= x $0.0e0)
          (values (float $0.0e0 original-x) 1)
          (let* ((ex (locally (declare (optimize (safety 0)))
                       (the fixnum
                         (round (* exponent
                                   ;; this is the closest double float
                                   ;; to (log 2 10), but expressed so
                                   ;; that we're not vulnerable to the
                                   ;; host lisp's interpretation of
                                   ;; arithmetic.  (FIXME: it turns
                                   ;; out that sbcl itself is off by 1
                                   ;; ulp in this value, which is a
                                   ;; little unfortunate.)
                                    #-long-float
                                    (make-double-float 1070810131 1352628735)
                                    #+long-float
                                    (error "(log 2 10) not computed"))))))
                 (x (if (minusp ex)
                        (if (float-denormalized-p x)
                            #-long-float
                            (* x $1.0e16 (expt $10.0e0 (- (- ex) 16)))
                            #+long-float
                            (* x $1.0e18 (expt $10.0e0 (- (- ex) 18)))
                            (* x $10.0e0 (expt $10.0e0 (- (- ex) 1))))
                        (/ x $10.0e0 (expt $10.0e0 (1- ex))))))
            (do ((d $10.0e0 (* d $10.0e0))
                 (y x (/ x d))
                 (ex ex (1+ ex)))
                ((< y $1.0e0)
                 (do ((m $10.0e0 (* m $10.0e0))
                      (z y (* y m))
                      (ex ex (1- ex)))
                     ((>= z $0.1e0)
                      (values (float z original-x) ex))
                   (declare (long-float m) (integer ex))))
              (declare (long-float d))))))))
(eval-when (:compile-toplevel :execute)
  (setf *read-default-float-format* 'cl:single-float))

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
;;;
;;; NOTE II: this has been bypassed slightly by implementing Burger
;;; and Dybvig, 1996.  When someone has time (KLUDGE) they can
;;; probably (a) implement the optimizations suggested by Burger and
;;; Dyvbig, and (b) remove all vestiges of Dragon4, including from
;;; fixed-format printing.

;;; Print the appropriate exponent marker for X and the specified exponent.
(defun print-float-exponent (x exp stream)
  (declare (type float x) (type integer exp) (type stream stream))
  (cond ((case *read-default-float-format*
           ((short-float single-float)
            (typep x 'single-float))
           ((double-float #-long-float long-float)
            (typep x 'double-float))
           #+long-float
           (long-float
            (typep x 'long-float)))
         (unless (eql exp 0)
           (write-char #\e stream)
           (%output-integer-in-base exp 10 stream)))
        (t
         (write-char
          (etypecase x
            (single-float #\f)
            (double-float #\d)
            (short-float #\s)
            (long-float #\L))
          stream)
         (%output-integer-in-base exp 10 stream))))

(defmethod print-object ((x float) stream)
  (cond
    ((float-infinity-or-nan-p x)
     (if (float-infinity-p x)
         (let ((symbol (etypecase x
                         (single-float (if (minusp x)
                                           'single-float-negative-infinity
                                           'single-float-positive-infinity))
                         (double-float (if (minusp x)
                                           'double-float-negative-infinity
                                           'double-float-positive-infinity)))))
           (cond (*read-eval*
                  (write-string "#." stream)
                  (output-symbol symbol (sb-xc:symbol-package symbol) stream))
                 (t
                  (print-unreadable-object (x stream)
                    (output-symbol symbol (sb-xc:symbol-package symbol) stream)))))
         (print-unreadable-object (x stream)
           (princ (float-format-name x) stream)
           (write-string (if (float-trapping-nan-p x) " trapping" " quiet") stream)
           (write-string " NaN" stream))))
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
          (print-float x stream)))))))




;;;; other leaf objects

;;; If *PRINT-ESCAPE* is false, just do a WRITE-CHAR, otherwise output
;;; the character name or the character in the #\char format.
(defmethod print-object ((char character) stream)
  (if (or *print-escape* *print-readably*)
      (let ((graphicp (and (graphic-char-p char)
                           (standard-char-p char)))
            (name (char-name char)))
        (write-string "#\\" stream)
        (if (and name (or (not graphicp) *print-readably*))
            (quote-string name stream)
            (write-char char stream)))
      (write-char char stream)))

(defmethod print-object ((sap system-area-pointer) stream)
  (cond (*read-eval*
         (format stream "#.(~S #X~8,'0X)" 'int-sap (sap-int sap)))
        (t
         (print-unreadable-object (sap stream)
           (format stream "system area pointer: #X~8,'0X" (sap-int sap))))))

(defmethod print-object ((weak-pointer weak-pointer) stream)
  (print-unreadable-object (weak-pointer stream)
    (multiple-value-bind (value validp) (weak-pointer-value weak-pointer)
      (cond (validp
             (write-string "weak pointer: " stream)
             (write value :stream stream))
            (t
             (write-string "broken weak pointer" stream))))))

(defmethod print-object ((component code-component) stream)
  (print-unreadable-object (component stream :identity t)
    (let (dinfo)
      (cond ((code-obj-is-filler-p component)
             (format stream "filler ~dw"
                     (ash (code-object-size component) (- sb-vm:word-shift))))
            ((eq (setq dinfo (%code-debug-info component)) :bpt-lra)
             (write-string "bpt-trap-return" stream))
            ((functionp dinfo)
             (format stream "trampoline ~S" dinfo))
            (t
             (format stream "code~@[ id=~x~] [~D]"
                     (%code-serialno component)
                     (code-n-entries component))
             (let ((fun-name (awhen (%code-entry-point component 0)
                               (%simple-fun-name it))))
               (when fun-name
                 (write-char #\Space stream)
                 (write fun-name :stream stream))
               (cond ((not (typep dinfo 'sb-c::debug-info)))
                     ((neq (sb-c::debug-info-name dinfo) fun-name)
                      (write-string ", " stream)
                      (output-object (sb-c::debug-info-name dinfo) stream)))))))))

#-(or x86 x86-64)
(defmethod print-object ((lra lra) stream)
  (print-unreadable-object (lra stream :identity t)
    (write-string "return PC object" stream)))

(defmethod print-object ((fdefn fdefn) stream)
  (print-unreadable-object (fdefn stream :type t)
    ;; As fdefn names are particularly relevant to those hacking on the compiler
    ;; and disassembler, be maximally helpful by neither abbreviating (SETF ...)
    ;; due to length cutoff, nor failing to print a package if needed.
    ;; Some folks seem to love same-named symbols way too much.
    (let ((*print-length* 20)) ; arbitrary
      (prin1 (fdefn-name fdefn) stream))))

#+sb-simd-pack
(defmethod print-object ((pack simd-pack) stream)
  (cond ((and *print-readably* *read-eval*)
         (multiple-value-bind (format maker extractor)
             (etypecase pack
               ((simd-pack double-float)
                (values "#.(~S ~S ~S)"
                        '%make-simd-pack-double #'%simd-pack-doubles))
               ((simd-pack single-float)
                (values "#.(~S ~S ~S ~S ~S)"
                        '%make-simd-pack-single #'%simd-pack-singles))
               (t
                (values "#.(~S #X~16,'0X #X~16,'0X)"
                        '%make-simd-pack-ub64 #'%simd-pack-ub64s)))
           (multiple-value-call
               #'format stream format maker (funcall extractor pack))))
        (*print-readably*
         (print-not-readable-error pack stream))
        (t
         (print-unreadable-object (pack stream)
           (flet ((all-ones-p (value start end &aux (mask (- (ash 1 end) (ash 1 start))))
                    (= (logand value mask) mask))
                  (split-num (value start)
                    (loop
                       for i from 0 to 3
                       and v = (ash value (- start)) then (ash v -8)
                       collect (logand v #xFF))))
             (multiple-value-bind (low high)
                 (%simd-pack-ub64s pack)
               (etypecase pack
                 ((simd-pack double-float)
                  (multiple-value-bind (v0 v1) (%simd-pack-doubles pack)
                    (format stream "~S~@{ ~:[~,13E~;~*TRUE~]~}"
                            'simd-pack
                            (all-ones-p low 0 64) v0
                            (all-ones-p high 0 64) v1)))
                 ((simd-pack single-float)
                  (multiple-value-bind (v0 v1 v2 v3) (%simd-pack-singles pack)
                    (format stream "~S~@{ ~:[~,7E~;~*TRUE~]~}"
                            'simd-pack
                            (all-ones-p low 0 32) v0
                            (all-ones-p low 32 64) v1
                            (all-ones-p high 0 32) v2
                            (all-ones-p high 32 64) v3)))
                 (t
                  (format stream "~S~@{ ~{ ~2,'0X~}~}"
                          'simd-pack
                          (split-num low 0) (split-num low 32)
                          (split-num high 0) (split-num high 32))))))))))

#+sb-simd-pack-256
(defmethod print-object ((pack simd-pack-256) stream)
  (cond ((and *print-readably* *read-eval*)
         (multiple-value-bind (format maker extractor)
             (etypecase pack
               ((simd-pack-256 double-float)
                (values "#.(~@{~S~^ ~})"
                        '%make-simd-pack-256-double #'%simd-pack-256-doubles))
               ((simd-pack-256 single-float)
                (values "#.(~@{~S~^ ~})"
                        '%make-simd-pack-256-single #'%simd-pack-256-singles))
               (t
                (values "#.(~S~@{ #X~16,'0X~})"
                        '%make-simd-pack-256-ub64 #'%simd-pack-256-ub64s)))
           (multiple-value-call
               #'format stream format maker (funcall extractor pack))))
        (*print-readably*
         (print-not-readable-error pack stream))
        (t
         (print-unreadable-object (pack stream)
           (etypecase pack
             ((simd-pack-256 double-float)
              (multiple-value-call #'format stream "~S~@{ ~,13E~}"
                'simd-pack-256
                (%simd-pack-256-doubles pack)))
             ((simd-pack-256 single-float)
              (multiple-value-call #'format stream "~S~@{ ~,7E~}"
                'simd-pack-256
                (%simd-pack-256-singles pack)))
             (t
              (multiple-value-bind (p0 p1 p2 p3)
                  (%simd-pack-256-ub64s pack)
                (format stream "~S~@{ ~16,'0X~}"
                        'simd-pack-256
                        p0 p1 p2 p3))))))))

;;;; functions

(defmethod print-object ((object function) stream)
  (let* ((name (%fun-name object))
         (proper-name-p (and (legal-fun-name-p name) (fboundp name)
                             (eq (fdefinition name) object))))
    ;; ":TYPE T" is no good, since CLOSURE doesn't have full-fledged status.
    (print-unreadable-object (object stream :identity (not proper-name-p))
      (format stream "~A~@[ ~S~]"
              ;; TYPE-OF is so that GFs print as #<STANDARD-GENERIC-FUNCTION>
              ;; and not #<FUNCTION> before SRC;PCL;PRINT-OBJECT is loaded.
              (if (closurep object) 'closure (type-of object))
              name))))

;;;; catch-all for unknown things

(declaim (inline lowtag-of))
(defun lowtag-of (x) (logand (get-lisp-obj-address x) sb-vm:lowtag-mask))

(defmethod print-object ((object t) stream)
  (when (eq object sb-pcl:+slot-unbound+)
    ;; If specifically the unbound marker with 0 data,
    ;; as opposed to any other unbound marker.
    (print-unreadable-object (object stream) (write-string "unbound" stream))
    (return-from print-object))
  (print-unreadable-object (object stream :identity t)
    (let ((lowtag (lowtag-of object)))
      (case lowtag
        (#.sb-vm:other-pointer-lowtag
         (let ((widetag (widetag-of object)))
           (case widetag
             (#.sb-vm:value-cell-widetag
              (write-string "value cell " stream)
              (output-object (value-cell-ref object) stream))
             (#.sb-vm:filler-widetag
              (write-string "pad " stream)
              (write (1+ (get-header-data object)) :stream stream)
              (write-string "w" stream)) ; words
             (t
              (write-string "unknown pointer object, widetag=" stream)
              (output-integer widetag stream 16 t)))))
        ((#.sb-vm:fun-pointer-lowtag
          #.sb-vm:instance-pointer-lowtag
          #.sb-vm:list-pointer-lowtag)
         (write-string "unknown pointer object, lowtag=" stream)
         (output-integer lowtag stream 16 t))
        (t
         (case (widetag-of object)
           (#.sb-vm:unbound-marker-widetag
            (write-string "unbound marker" stream))
           (t
            (write-string "unknown immediate object, lowtag=" stream)
            (output-integer lowtag stream 2 t)
            (write-string ", widetag=" stream)
            (output-integer (widetag-of object) stream 16 t))))))))
