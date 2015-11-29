;;;; Gray streams implementation for SBCL, based on the Gray streams
;;;; implementation for CMU CL, based on the stream-definition-by-user
;;;; proposal by David N. Gray.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB-GRAY")

;;; BUG-OR-ERROR: because we have extensible streams, wherewith the
;;; user is responsible for some of the protocol implementation, it's
;;; not necessarily a bug in SBCL itself if we fall through to one of
;;; these default methods.
;;;
;;; FIXME: there's a lot of similarity in these Gray stream
;;; implementation generic functions.  All of them could (maybe
;;; should?) have two default methods: one on STREAM calling
;;; BUG-OR-ERROR, and one on T signalling a TYPE-ERROR.
(defmacro bug-or-error (stream fun)
  `(error
    "~@<The stream ~S has no suitable method for ~S, ~
     and so has fallen through to this method.  If you think that this is ~
     a bug, please report it to the applicable authority (bugs in SBCL itself ~
     should go to the mailing lists referenced from ~
     <http://www.sbcl.org/>).~@:>"
    ,stream ,fun))

(fmakunbound 'stream-element-type)

(defgeneric stream-element-type (stream)
  #+sb-doc
  (:documentation
   "Return a type specifier for the kind of object returned by the
  STREAM. The class FUNDAMENTAL-CHARACTER-STREAM provides a default method
  which returns CHARACTER."))

(defmethod stream-element-type ((stream ansi-stream))
  (ansi-stream-element-type stream))

(defmethod stream-element-type ((stream fundamental-character-stream))
  'character)

(defmethod stream-element-type ((stream stream))
  (bug-or-error stream 'stream-element-type))

(defmethod stream-element-type ((non-stream t))
  (error 'type-error :datum non-stream :expected-type 'stream))

(defgeneric pcl-open-stream-p (stream)
  #+sb-doc
  (:documentation
   "Return true if STREAM is not closed. A default method is provided
  by class FUNDAMENTAL-STREAM which returns true if CLOSE has not been
  called on the stream."))

(defmethod pcl-open-stream-p ((stream ansi-stream))
  (ansi-stream-open-stream-p stream))

(defmethod pcl-open-stream-p ((stream fundamental-stream))
  (stream-open-p stream))

(defmethod pcl-open-stream-p ((stream stream))
  (bug-or-error stream 'open-stream-p))

(defmethod pcl-open-stream-p ((non-stream t))
  (error 'type-error :datum non-stream :expected-type 'stream))

;;; bootstrapping hack
(pcl-open-stream-p (make-string-output-stream))
(setf (fdefinition 'open-stream-p) #'pcl-open-stream-p)

(defgeneric pcl-close (stream &key abort)
  #+sb-doc
  (:documentation
   "Close the given STREAM. No more I/O may be performed, but
  inquiries may still be made. If :ABORT is true, an attempt is made
  to clean up the side effects of having created the stream."))

(defmethod pcl-close ((stream ansi-stream) &key abort)
  (ansi-stream-close stream abort))

(defmethod pcl-close ((stream fundamental-stream) &key abort)
  (declare (ignore abort))
  (setf (stream-open-p stream) nil)
  t)

(progn
  ;; KLUDGE: Get in a call to PCL-CLOSE with a string-output-stream before
  ;; setting it as CLOSE. Otherwise using NAMED-LAMBDAs as DFUNs causes a
  ;; vicious metacircle from FORMAT NIL somewhere in the compiler. This is
  ;; enough to get the dispatch settled down before we need it.
  (pcl-close (make-string-output-stream))
  (setf (fdefinition 'close) #'pcl-close))

(let ()
  (fmakunbound 'input-stream-p)

  (defgeneric input-stream-p (stream)
    #+sb-doc
    (:documentation "Can STREAM perform input operations?"))

  (defmethod input-stream-p ((stream ansi-stream))
    (ansi-stream-input-stream-p stream))

  (defmethod input-stream-p ((stream fundamental-stream))
    nil)

  (defmethod input-stream-p ((stream fundamental-input-stream))
    t)

  (defmethod input-stream-p ((stream stream))
    (bug-or-error stream 'input-stream-p))

  (defmethod input-stream-p ((non-stream t))
    (error 'type-error :datum non-stream :expected-type 'stream)))

(let ()
  (fmakunbound 'interactive-stream-p)

  (defgeneric interactive-stream-p (stream)
    #+sb-doc
    (:documentation "Is STREAM an interactive stream?"))

  (defmethod interactive-stream-p ((stream ansi-stream))
    (funcall (ansi-stream-misc stream) stream :interactive-p))

  (defmethod interactive-stream-p ((stream fundamental-stream))
    nil)

  (defmethod interactive-stream-p ((stream stream))
    (bug-or-error stream 'interactive-stream-p))

  (defmethod interactive-stream-p ((non-stream t))
    (error 'type-error :datum non-stream :expected-type 'stream)))

(let ()
  (fmakunbound 'output-stream-p)

  (defgeneric output-stream-p (stream)
    #+sb-doc
    (:documentation "Can STREAM perform output operations?"))

  (defmethod output-stream-p ((stream ansi-stream))
    (ansi-stream-output-stream-p stream))

  (defmethod output-stream-p ((stream fundamental-stream))
    nil)

  (defmethod output-stream-p ((stream fundamental-output-stream))
    t)

  (defmethod output-stream-p ((stream stream))
    (bug-or-error stream 'output-stream-p))

  (defmethod output-stream-p ((non-stream t))
    (error 'type-error :datum non-stream :expected-type 'stream)))

;;; character input streams
;;;
;;; A character input stream can be created by defining a class that
;;; includes FUNDAMENTAL-CHARACTER-INPUT-STREAM and defining methods
;;; for the generic functions below.

(defgeneric stream-read-char (stream)
  #+sb-doc
  (:documentation
   "Read one character from the stream. Return either a
  character object, or the symbol :EOF if the stream is at end-of-file.
  Every subclass of FUNDAMENTAL-CHARACTER-INPUT-STREAM must define a
  method for this function."))

(defgeneric stream-unread-char (stream character)
  #+sb-doc
  (:documentation
   "Undo the last call to STREAM-READ-CHAR, as in UNREAD-CHAR.
  Return NIL. Every subclass of FUNDAMENTAL-CHARACTER-INPUT-STREAM
  must define a method for this function."))

(defgeneric stream-read-char-no-hang (stream)
  #+sb-doc
  (:documentation
   "This is used to implement READ-CHAR-NO-HANG. It returns either a
  character, or NIL if no input is currently available, or :EOF if
  end-of-file is reached. The default method provided by
  FUNDAMENTAL-CHARACTER-INPUT-STREAM simply calls STREAM-READ-CHAR; this
  is sufficient for file streams, but interactive streams should define
  their own method."))

(defmethod stream-read-char-no-hang ((stream fundamental-character-input-stream))
  (stream-read-char stream))

(defgeneric stream-peek-char (stream)
  #+sb-doc
  (:documentation
   "This is used to implement PEEK-CHAR; this corresponds to PEEK-TYPE of NIL.
  It returns either a character or :EOF. The default method calls
  STREAM-READ-CHAR and STREAM-UNREAD-CHAR."))

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((char (stream-read-char stream)))
    (unless (eq char :eof)
      (stream-unread-char stream char))
    char))

(defgeneric stream-listen (stream)
  #+sb-doc
  (:documentation
   "This is used by LISTEN. It returns true or false. The default method uses
  STREAM-READ-CHAR-NO-HANG and STREAM-UNREAD-CHAR. Most streams should
  define their own method since it will usually be trivial and will
  always be more efficient than the default method."))

(defmethod stream-listen ((stream fundamental-character-input-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (when (characterp char)
      (stream-unread-char stream char)
      t)))

(defgeneric stream-read-line (stream)
  #+sb-doc
  (:documentation
   "This is used by READ-LINE. A string is returned as the first value. The
  second value is true if the string was terminated by end-of-file
  instead of the end of a line. The default method uses repeated
  calls to STREAM-READ-CHAR."))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (let ((res (make-string 80))
        (len 80)
        (index 0))
    (loop
     (let ((ch (stream-read-char stream)))
       (cond ((eq ch :eof)
              (return (values (%shrink-vector res index) t)))
             (t
              (when (char= ch #\newline)
                (return (values (%shrink-vector res index) nil)))
              (when (= index len)
                (setq len (* len 2))
                (let ((new (make-string len)))
                  (replace new res)
                  (setq res new)))
              (setf (schar res index) ch)
              (incf index)))))))

(defgeneric stream-clear-input (stream)
  #+sb-doc
  (:documentation
   "This is like CL:CLEAR-INPUT, but for Gray streams, returning NIL.
  The default method does nothing."))

(defmethod stream-clear-input ((stream fundamental-character-input-stream))
  nil)
(defmethod stream-clear-input ((stream stream))
  (bug-or-error stream 'stream-clear-input))
(defmethod stream-clear-input ((non-stream t))
  (error 'type-error :datum non-stream :expected-type 'stream))

(defgeneric stream-read-sequence (stream seq &optional start end)
  #+sb-doc
  (:documentation
   "This is like CL:READ-SEQUENCE, but for Gray streams."))

(defmethod stream-read-sequence ((stream fundamental-character-input-stream)
                                 (seq sequence)
                                 &optional (start 0) (end nil))
  (sb-impl::read-sequence/read-function
   seq stream start end 'character
   (lambda (stream eof-error-p eof-value recursive-p)
     (aver (null eof-error-p))
     (aver (eq :eof eof-value))
     (aver (not recursive-p))
     (stream-read-char stream))
   #'ill-bin))

(defmethod stream-read-sequence ((stream fundamental-binary-input-stream)
                                 (seq sequence)
                                 &optional (start 0) (end nil))
  (let ((stream-element-mode (sb-impl::stream-element-type-stream-element-mode
                              (stream-element-type stream))))
    (sb-impl::read-sequence/read-function
     seq stream start end stream-element-mode
     #'ill-in
     (lambda (stream eof-error-p eof-value recursive-p)
       (aver (null eof-error-p))
       (aver (eq :eof eof-value))
       (aver (not recursive-p))
       (stream-read-byte stream)))))


;;; character output streams
;;;
;;; A character output stream can be created by defining a class that
;;; includes FUNDAMENTAL-CHARACTER-OUTPUT-STREAM and defining methods
;;; for the generic functions below.

(defgeneric stream-write-char (stream character)
  #+sb-doc
  (:documentation
   "Write CHARACTER to STREAM and return CHARACTER. Every
  subclass of FUNDAMENTAL-CHARACTER-OUTPUT-STREAM must have a method
  defined for this function."))

(defgeneric stream-line-column (stream)
  (:method ((stream sb-int:form-tracking-stream))
    (cdr (sb-int:line/col-from-charpos stream)))
  #+sb-doc
  (:documentation
   "Return the column number where the next character
  will be written, or NIL if that is not meaningful for this stream.
  The first column on a line is numbered 0. This function is used in
  the implementation of PPRINT and the FORMAT ~T directive. For every
  character output stream class that is defined, a method must be
  defined for this function, although it is permissible for it to
  always return NIL."))

(defmethod stream-line-column ((stream fundamental-character-output-stream))
   nil)

;;; STREAM-LINE-LENGTH is a CMU CL extension to Gray streams.
;;; FIXME: Should we support it? Probably not..
(defgeneric stream-line-length (stream)
  #+sb-doc
  (:documentation "Return the stream line length or NIL."))

(defmethod stream-line-length ((stream fundamental-character-output-stream))
  nil)

(defgeneric stream-start-line-p (stream)
  #+sb-doc
  (:documentation
   "Is STREAM known to be positioned at the beginning of a line?
  It is permissible for an implementation to always return
  NIL. This is used in the implementation of FRESH-LINE. Note that
  while a value of 0 from STREAM-LINE-COLUMN also indicates the
  beginning of a line, there are cases where STREAM-START-LINE-P can be
  meaningfully implemented although STREAM-LINE-COLUMN can't be. For
  example, for a window using variable-width characters, the column
  number isn't very meaningful, but the beginning of the line does have
  a clear meaning. The default method for STREAM-START-LINE-P on class
  FUNDAMENTAL-CHARACTER-OUTPUT-STREAM uses STREAM-LINE-COLUMN, so if
  that is defined to return NIL, then a method should be provided for
  either STREAM-START-LINE-P or STREAM-FRESH-LINE."))

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (eql (stream-line-column stream) 0))

(defgeneric stream-write-string (stream string &optional start end)
  #+sb-doc
  (:documentation
   "This is used by WRITE-STRING. It writes the string to the stream,
  optionally delimited by start and end, which default to 0 and NIL.
  The string argument is returned. The default method provided by
  FUNDAMENTAL-CHARACTER-OUTPUT-STREAM uses repeated calls to
  STREAM-WRITE-CHAR."))

(defmethod stream-write-string ((stream fundamental-character-output-stream)
                                string &optional (start 0) end)
  (with-array-data ((data string) (offset-start start) (offset-end end)
                    :check-fill-pointer t)
    (sb-impl::write-sequence/vector
     (data simple-string) stream offset-start offset-end #'stream-write-char))
  string)

(defgeneric stream-terpri (stream)
  #+sb-doc
  (:documentation
   "Writes an end of line, as for TERPRI. Returns NIL. The default
  method does (STREAM-WRITE-CHAR stream #\NEWLINE)."))

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (stream-write-char stream #\Newline))

(defgeneric stream-fresh-line (stream)
  #+sb-doc
  (:documentation
   "Outputs a new line to the Stream if it is not positioned at the
  beginning of a line. Returns T if it output a new line, nil
  otherwise. Used by FRESH-LINE. The default method uses
  STREAM-START-LINE-P and STREAM-TERPRI."))

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defgeneric stream-finish-output (stream)
  #+sb-doc
  (:documentation
   "Attempts to ensure that all output sent to the Stream has reached
  its destination, and only then returns false. Implements
  FINISH-OUTPUT. The default method does nothing."))

(defmethod stream-finish-output ((stream fundamental-output-stream))
  nil)
(defmethod stream-finish-output ((stream stream))
  (bug-or-error stream 'stream-finish-output))
(defmethod stream-finish-output ((non-stream t))
  (error 'type-error :datum non-stream :expected-type 'stream))

(defgeneric stream-force-output (stream)
  #+sb-doc
  (:documentation
   "Attempts to force any buffered output to be sent. Implements
  FORCE-OUTPUT. The default method does nothing."))

(defmethod stream-force-output ((stream fundamental-output-stream))
  nil)
(defmethod stream-force-output ((stream stream))
  (bug-or-error stream 'stream-force-output))
(defmethod stream-force-output ((non-stream t))
  (error 'type-error :datum non-stream :expected-type 'stream))

(defgeneric stream-clear-output (stream)
  #+sb-doc
  (:documentation
   "This is like CL:CLEAR-OUTPUT, but for Gray streams: clear the given
  output STREAM. The default method does nothing."))

(defmethod stream-clear-output ((stream fundamental-output-stream))
  nil)
(defmethod stream-clear-output ((stream stream))
  (bug-or-error stream 'stream-clear-output))
(defmethod stream-clear-output ((non-stream t))
  (error 'type-error :datum non-stream :expected-type 'stream))

(defgeneric stream-advance-to-column (stream column)
  #+sb-doc
  (:documentation
   "Write enough blank space so that the next character will be
  written at the specified column. Returns true if the operation is
  successful, or NIL if it is not supported for this stream. This is
  intended for use by by PPRINT and FORMAT ~T. The default method uses
  STREAM-LINE-COLUMN and repeated calls to STREAM-WRITE-CHAR with a
  #\SPACE character; it returns NIL if STREAM-LINE-COLUMN returns NIL."))

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream)
                                     column)
  (let ((current-column (stream-line-column stream)))
    (when current-column
      (let ((fill (- column current-column)))
        (dotimes (i fill)
          (stream-write-char stream #\Space)))
      T)))

(defgeneric stream-write-sequence (stream seq &optional start end)
  #+sb-doc
  (:documentation
   "This is like CL:WRITE-SEQUENCE, but for Gray streams."))

(defmethod stream-write-sequence ((stream fundamental-character-output-stream)
                                  (seq sequence)
                                  &optional (start 0) (end nil))
  (sb-impl::write-sequence/write-function
   seq stream start end 'character #'stream-write-char #'ill-bout))

;; Provide a reasonable default for binary Gray streams.  We might be
;; able to do better by specializing on the sequence type, but at
;; least the behaviour is reasonable. --tony 2003/05/08.
(defmethod stream-write-sequence ((stream fundamental-binary-output-stream)
                                  (seq sequence)
                                  &optional (start 0) (end nil))
  (let ((stream-element-mode (sb-impl::stream-element-type-stream-element-mode
                              (stream-element-type stream))))
    (sb-impl::write-sequence/write-function
     seq stream start end stream-element-mode
     #'ill-out #'stream-write-byte)))


;;; binary streams
;;;
;;; Binary streams can be created by defining a class that includes
;;; either FUNDAMENTAL-BINARY-INPUT-STREAM or
;;; FUNDAMENTAL-BINARY-OUTPUT-STREAM (or both) and defining a method
;;; for STREAM-ELEMENT-TYPE and for one or both of the following
;;; generic functions.

(defgeneric stream-read-byte (stream)
  #+sb-doc
  (:documentation
   "Used by READ-BYTE; returns either an integer, or the symbol :EOF
  if the stream is at end-of-file."))

(defmethod stream-read-byte ((stream stream))
  (bug-or-error stream 'stream-read-byte))
(defmethod stream-read-byte ((non-stream t))
  (error 'type-error :datum non-stream :expected-type 'stream))

(defgeneric stream-write-byte (stream integer)
  #+sb-doc
  (:documentation
   "Implements WRITE-BYTE; writes the integer to the stream and
  returns the integer as the result."))

(defmethod stream-write-byte ((stream stream) integer)
  (bug-or-error stream 'stream-write-byte))
(defmethod stream-write-byte ((non-stream t) integer)
  (error 'type-error :datum non-stream :expected-type 'stream))

(defgeneric stream-file-position (stream &optional position-spec)
  #+sb-doc
  (:documentation
   "Used by FILE-POSITION. Returns or changes the current position within STREAM."))

(defmethod stream-file-position ((stream ansi-stream) &optional position-spec)
  (ansi-stream-file-position stream position-spec))

(defmethod stream-file-position ((stream t) &optional position-spec)
  (declare (ignore stream position-spec))
  nil)


;;; This is not in the Gray stream proposal, so it is left here
;;; as example code.
#|
;;; example character output stream encapsulating a lisp-stream
(defun make-character-output-stream (lisp-stream)
  (declare (type lisp-stream lisp-stream))
  (make-instance 'character-output-stream :lisp-stream lisp-stream))

(defmethod open-stream-p ((stream character-output-stream))
  (open-stream-p (character-output-stream-lisp-stream stream)))

(defmethod close ((stream character-output-stream) &key abort)
  (close (character-output-stream-lisp-stream stream) :abort abort))

(defmethod input-stream-p ((stream character-output-stream))
  (input-stream-p (character-output-stream-lisp-stream stream)))

(defmethod output-stream-p ((stream character-output-stream))
  (output-stream-p (character-output-stream-lisp-stream stream)))

(defmethod stream-write-char ((stream character-output-stream) character)
  (write-char character (character-output-stream-lisp-stream stream)))

(defmethod stream-line-column ((stream character-output-stream))
  (charpos (character-output-stream-lisp-stream stream)))

(defmethod stream-line-length ((stream character-output-stream))
  (line-length (character-output-stream-lisp-stream stream)))

(defmethod stream-finish-output ((stream character-output-stream))
  (finish-output (character-output-stream-lisp-stream stream)))

(defmethod stream-force-output ((stream character-output-stream))
  (force-output (character-output-stream-lisp-stream stream)))

(defmethod stream-clear-output ((stream character-output-stream))
  (clear-output (character-output-stream-lisp-stream stream)))

;;; example character input stream encapsulating a lisp-stream

(defun make-character-input-stream (lisp-stream)
  (declare (type lisp-stream lisp-stream))
  (make-instance 'character-input-stream :lisp-stream lisp-stream))

(defmethod open-stream-p ((stream character-input-stream))
  (open-stream-p (character-input-stream-lisp-stream stream)))

(defmethod close ((stream character-input-stream) &key abort)
  (close (character-input-stream-lisp-stream stream) :abort abort))

(defmethod input-stream-p ((stream character-input-stream))
  (input-stream-p (character-input-stream-lisp-stream stream)))

(defmethod output-stream-p ((stream character-input-stream))
  (output-stream-p (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char ((stream character-input-stream))
  (read-char (character-input-stream-lisp-stream stream) nil :eof))

(defmethod stream-unread-char ((stream character-input-stream) character)
  (unread-char character (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char-no-hang ((stream character-input-stream))
  (read-char-no-hang (character-input-stream-lisp-stream stream) nil :eof))

#+nil
(defmethod stream-peek-char ((stream character-input-stream))
  (peek-char nil (character-input-stream-lisp-stream stream) nil :eof))

#+nil
(defmethod stream-listen ((stream character-input-stream))
  (listen (character-input-stream-lisp-stream stream)))

(defmethod stream-clear-input ((stream character-input-stream))
  (clear-input (character-input-stream-lisp-stream stream)))
|#
