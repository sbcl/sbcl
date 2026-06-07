(in-package :sb-manual)

(defsection @streams (:title "Streams")
  "Streams which read or write Lisp character data from or to the outside
  world -- files, sockets or other external entities -- require the
  specification of a conversion between the external, binary data and
  the Lisp characters. In ANSI Common Lisp, this is done by specifying
  the :EXTERNAL-FORMAT argument when the stream is created. The major
  information required is an _encoding_, specified by a keyword naming
  that encoding; however, it is also possible to specify refinements
  to that encoding as additional options to the external format
  designator.

  In addition, SBCL supports various extensions of ANSI Common Lisp
  streams:

  - _Bivalent Streams_: A type of stream that can read and write both
    CHARACTER and `(UNSIGNED-BYTE 8)` values.

  - _Gray Streams_: User-overloadable CLOS classes whose instances can
    be used as Lisp streams (e.g. passed as the first argument to
    FORMAT).

  - _Simple Streams_: The bundled contrib module `SB-SIMPLE-STREAMS`
    implements a subset of the Franz Allegro simple-streams proposal."
  (@stream-external-formats section)
  (@bivalent-streams section)
  (@gray-streams section)
  (@sb-simple-streams section))

(defsection @stream-external-formats (:title "Stream External Formats")
  "The function STREAM-EXTERNAL-FORMAT returns the canonical name of
  the external format (See @EXTERNAL-FORMATS) used by the stream for
  character-based input and/or output.

  When constructing file streams, for example using OPEN or
  WITH-OPEN-FILE, the external format to use is specified via the
  :EXTERNAL-FORMAT argument which accepts an external format
  designator (see @EXTERNAL-FORMAT-DESIGNATORS).")

(defsection @bivalent-streams (:title "Bivalent Streams")
  "A _bivalent stream_ can be used to read and write both
  CHARACTER and `(UNSIGNED-BYTE 8)` values. A bivalent stream is
  created by calling OPEN with the argument :ELEMENT-TYPE
  :DEFAULT. On such a stream, both binary and character data can be
  read and written with the usual input and output functions.

  Streams are _not_ created bivalent by default for performance
  reasons. Bivalent streams are incompatible with `FAST-READ-CHAR`, an
  internal optimization in SBCL's stream machinery that bulk-converts
  octets to characters and implements a fast path through READ-CHAR.")

(defsection @gray-streams (:title "Gray Streams")
  "The Gray Streams interface is a widely supported extension that
  provides for definition of CLOS-extensible stream classes. Gray
  stream classes are implemented by adding methods to generic
  functions analogous to Common Lisp's standard I/O functions.
  Instances of Gray stream classes may be used with any I/O operation
  where a non-Gray stream can, provided that all required methods have
  been implemented suitably."
  (@gray-streams-classes section)
  (@methods-common-to-all-streams section)
  (@input-stream-methods section)
  (@character-input-stream-methods section)
  (@output-stream-methods section)
  (@character-output-stream-methods section)
  (@binary-stream-methods section)
  (@gray-streams-examples section))

(defsection @gray-streams-classes (:title "Gray Streams classes")
  "The defined Gray Stream classes are these:"
  (sb-gray:fundamental-stream class)
  (sb-gray:fundamental-input-stream class)
  "The function INPUT-STREAM-P will return true of any generalized
  instance of SB-GRAY:FUNDAMENTAL-INPUT-STREAM."
  (sb-gray:fundamental-output-stream class)
  "The function OUTPUT-STREAM-P will return true of any generalized
  instance of SB-GRAY:FUNDAMENTAL-OUTPUT-STREAM."
  (sb-gray:fundamental-binary-stream class)
  "Note that instantiable subclasses of SB-GRAY:FUNDAMENTAL-BINARY-STREAM
  should provide (or inherit) an applicable method for the generic
  function STREAM-ELEMENT-TYPE."
  (sb-gray:fundamental-character-stream class)
  (sb-gray:fundamental-binary-input-stream class)
  (sb-gray:fundamental-binary-output-stream class)
  (sb-gray:fundamental-character-input-stream class)
  (sb-gray:fundamental-character-output-stream class))

(defsection @methods-common-to-all-streams
    (:title "Methods common to all streams")
  "These generic functions can be specialized on any generalized instance
  of fundamental-stream."
  (stream-element-type generic-function)
  (close generic-function)
  (sb-gray:stream-file-position generic-function))

(defsection @input-stream-methods (:title "Input stream methods")
  "These generic functions may be specialized on any generalized instance
  of fundamental-input-stream."
  (sb-gray:stream-clear-input generic-function)
  (sb-gray:stream-read-sequence generic-function))

(defsection @character-input-stream-methods
    (:title "Character input stream methods")
  "These generic functions are used to implement subclasses of
  SB-GRAY:FUNDAMENTAL-INPUT-STREAM:"
  (sb-gray:stream-peek-char generic-function)
  (sb-gray:stream-read-char-no-hang generic-function)
  (sb-gray:stream-read-char generic-function)
  (sb-gray:stream-read-line generic-function)
  (sb-gray:stream-listen generic-function)
  (sb-gray:stream-unread-char generic-function))

(defsection @output-stream-methods (:title "Output stream methods")
  "These generic functions are used to implement subclasses of
  SB-GRAY:FUNDAMENTAL-OUTPUT-STREAM:"
  (sb-gray:stream-clear-output generic-function)
  (sb-gray:stream-finish-output generic-function)
  (sb-gray:stream-force-output generic-function)
  (sb-gray:stream-write-sequence generic-function))

(defsection @character-output-stream-methods
    (:title "Character output stream methods")
  "These generic functions are used to implement subclasses of
  SB-GRAY:FUNDAMENTAL-CHARACTER-OUTPUT-STREAM:"
  (sb-gray:stream-advance-to-column generic-function)
  (sb-gray:stream-fresh-line generic-function)
  (sb-gray:stream-line-column generic-function)
  (sb-gray:stream-line-length generic-function)
  (sb-gray:stream-start-line-p generic-function)
  (sb-gray:stream-terpri generic-function)
  (sb-gray:stream-write-char generic-function)
  (sb-gray:stream-write-string generic-function))

(defsection @binary-stream-methods (:title "Binary stream methods")
  "The following generic functions are available for subclasses of
  SB-GRAY:FUNDAMENTAL-BINARY-STREAM:"
  (sb-gray:stream-read-byte generic-function)
  (sb-gray:stream-write-byte generic-function))

(defsection @gray-streams-examples (:title "Gray Streams Examples")
  "Below are two classes of stream that can be conveniently defined as
  wrappers for Common Lisp streams. These are meant to serve as
  examples of minimal implementations of the protocols that must be
  followed when defining Gray streams. Realistic uses of the Gray
  Streams API would implement the various methods that can do I/O in
  batches, such as SB-GRAY:STREAM-READ-LINE,
  SB-GRAY:STREAM-WRITE-STRING, SB-GRAY:STREAM-READ-SEQUENCE, and
  SB-GRAY:STREAM-WRITE-SEQUENCE."
  (@character-counting-input-stream section)
  (@output-prefixing-character-stream section))

(defsection @character-counting-input-stream
    (:title "Character Counting Input Stream")
  "  It is occasionally handy for programs that process input files to
  count the number of characters and lines seen so far, and the number
  of characters seen on the current line, so that useful messages may
  be reported in case of parsing errors, etc. Here is a character
  input stream class that keeps track of these counts. Note that all
  character input streams must implement SB-GRAY:STREAM-READ-CHAR and
  SB-GRAY:STREAM-UNREAD-CHAR.

      (defclass wrapped-stream (fundamental-stream)
        ((stream :initarg :stream :reader stream-of)))

      (defmethod stream-element-type ((stream wrapped-stream))
        (stream-element-type (stream-of stream)))

      (defmethod close ((stream wrapped-stream) &key abort)
        (close (stream-of stream) :abort abort))

      (defclass wrapped-character-input-stream
          (wrapped-stream fundamental-character-input-stream)
        ())

      (defmethod stream-read-char ((stream wrapped-character-input-stream))
        (read-char (stream-of stream) nil :eof))

      (defmethod stream-unread-char ((stream wrapped-character-input-stream)
                                     char)
        (unread-char char (stream-of stream)))

      (defclass counting-character-input-stream
          (wrapped-character-input-stream)
        ((char-count :initform 1 :accessor char-count-of)
         (line-count :initform 1 :accessor line-count-of)
         (col-count :initform 1 :accessor col-count-of)
         (prev-col-count :initform 1 :accessor prev-col-count-of)))

      (defmethod stream-read-char ((stream counting-character-input-stream))
        (with-accessors ((inner-stream stream-of) (chars char-count-of)
                         (lines line-count-of) (cols col-count-of)
                         (prev prev-col-count-of)) stream
            (let ((char (call-next-method)))
              (cond ((eql char :eof)
                     :eof)
                    ((char= char #\Newline)
                     (incf lines)
                     (incf chars)
                     (setf prev cols)
                     (setf cols 1)
                     char)
                    (t
                     (incf chars)
                     (incf cols)
                     char)))))

      (defmethod stream-unread-char ((stream counting-character-input-stream)
                                     char)
        (with-accessors ((inner-stream stream-of) (chars char-count-of)
                         (lines line-count-of) (cols col-count-of)
                         (prev prev-col-count-of)) stream
            (cond ((char= char #\Newline)
                   (decf lines)
                   (decf chars)
                   (setf cols prev))
                  (t
                   (decf chars)
                   (decf cols)
                   char))
            (call-next-method)))

  The default methods for SB-GRAY:STREAM-READ-CHAR-NO-HANG,
  SB-GRAY:STREAM-PEEK-CHAR, SB-GRAY:STREAM-LISTEN,
  SB-GRAY:STREAM-CLEAR-INPUT, SB-GRAY:STREAM-READ-LINE, and
  SB-GRAY:STREAM-READ-SEQUENCE should be sufficient (though the last
  two will probably be slower than methods that forwarded directly).

  Here's a sample use of this class:

      (with-input-from-string (input \"1 2
       3 :foo  \")
        (let ((counted-stream (make-instance 'counting-character-input-stream
                               :stream input)))
          (loop for thing = (read counted-stream) while thing
             unless (numberp thing) do
               (error \"Non-number ~S (line ~D, column ~D)\" thing
                      (line-count-of counted-stream)
                      (- (col-count-of counted-stream)
                         (length (format nil \"~S\" thing))))
             end
             do (print thing))))

  Output:

      1
      2
      3
      Non-number :FOO (line 2, column 5)
        [Condition of type SIMPLE-ERROR]")

(defsection @output-prefixing-character-stream
    (:title "Output Prefixing Character Stream")
  "One use for a wrapped output stream might be to prefix each line of
  text with a timestamp, e.g. for a logging stream. Here's a simple
  stream that does this, though without any fancy line-wrapping. Note
  that all character output stream classes must implement
  SB-GRAY:STREAM-WRITE-CHAR and SB-GRAY:STREAM-LINE-COLUMN.

      (defclass wrapped-stream (fundamental-stream)
        ((stream :initarg :stream :reader stream-of)))

      (defmethod stream-element-type ((stream wrapped-stream))
        (stream-element-type (stream-of stream)))

      (defmethod close ((stream wrapped-stream) &key abort)
        (close (stream-of stream) :abort abort))

      (defclass wrapped-character-output-stream
          (wrapped-stream fundamental-character-output-stream)
        ((col-index :initform 0 :accessor col-index-of)))

      (defmethod stream-line-column ((stream wrapped-character-output-stream))
        (col-index-of stream))

      (defmethod stream-write-char ((stream wrapped-character-output-stream)
                                    char)
        (with-accessors ((inner-stream stream-of) (cols col-index-of)) stream
          (write-char char inner-stream)
          (if (char= char #\Newline)
              (setf cols 0)
              (incf cols))))

      (defclass prefixed-character-output-stream
          (wrapped-character-output-stream)
        ((prefix :initarg :prefix :reader prefix-of)))

      (defgeneric write-prefix (prefix stream)
        (:method ((prefix string) stream) (write-string prefix stream))
        (:method ((prefix function) stream) (funcall prefix stream)))

      (defmethod stream-write-char ((stream prefixed-character-output-stream)
                                    char)
        (with-accessors ((inner-stream stream-of) (cols col-index-of)
                         (prefix prefix-of)) stream
          (when (zerop cols)
            (write-prefix prefix inner-stream))
          (call-next-method)))

  As with the example input stream, this implements only the minimal
  protocol. A production implementation should also provide methods
  for at least SB-GRAY:STREAM-WRITE-STRING,
  SB-GRAY:STREAM-WRITE-SEQUENCE.

  And here's a sample use of this class:

      (flet ((format-timestamp (stream)
               (apply #'format stream \"[~2@*~2,' D:~1@*~2,'0D:~0@*~2,'0D] \"
                      (multiple-value-list (get-decoded-time)))))
        (let ((output (make-instance 'prefixed-character-output-stream
                                     :stream *standard-output*
                                     :prefix #'format-timestamp)))
          (loop for string in '(\"abc\" \"def\" \")ghi\") do
               (write-line string output)
               (sleep 1))))

  Output:

      [ 0:30:05] abc
      [ 0:30:06] def
      [ 0:30:07] ghi
      NIL")
