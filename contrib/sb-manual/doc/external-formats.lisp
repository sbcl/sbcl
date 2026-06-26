(in-package :sb-manual)

(defsection @external-formats (:title "External Formats"
                               :concepts (@external-format))
  "External formats determine the coding of characters from/to sequences
  of octets when exchanging data with the outside world. Examples of
  such exchanges are:

  - Character streams associated with files, sockets and process
    input/output (see @STREAM-EXTERNAL-FORMATS and
    @RUNNING-EXTERNAL-PROGRAMS)

  - Names of files

  - Foreign strings (see @FOREIGN-TYPES-AND-LISP-TYPES)

  - Posix interface (see @SB-POSIX)

  - Hostname- and protocol-related functions of the BSD-socket interface
    (see @NETWORKING)

  Technically, external formats in SBCL are named objects describing
  coding of characters as well as policies in case de- or encoding is
  not possible. Each external format has a canonical name and zero or
  more aliases. User code mostly interacts with external formats by
  supplying external format designators to functions that use external
  formats internally."
  (@default-external-format section)
  (@external-format-designators section)
  (@character-coding-conditions section)
  (@converting-between-strings-and-octet-vectors section)
  (@supported-external-formats section))

(defsection @default-external-format (:title "The Default External Format")
  (sb-ext:*default-external-format* variable)
  (sb-ext:*default-source-external-format* variable)
  ;; FIXME: Move this to @FFI?
  (sb-ext:*default-c-string-external-format* variable))

(defsection @external-format-designators (:title "External Format Designators")
  "In situations where an external format designator is required, such as
  the :EXTERNAL-FORMAT argument in calls to OPEN or WITH-OPEN-FILE,
  users may supply the name of an encoding to denote the external
  format which is applying that encoding to Lisp characters.

  In addition to the basic encoding for an external format, options
  controlling various special cases may be passed, by using a list
  (whose first element must be an encoding name and whose rest is a
  plist) as an external file format designator.

  More specifically, external format designators can take the
  following forms:

  - :DEFAULT: Designates the current default external format (see
    @DEFAULT-EXTERNAL-FORMAT).

  - `<keyword>`: Designates the supported external format that has
    `<keyword>` as one of its names (see @SUPPORTED-EXTERNAL-FORMATS).

  - `(<keyword> . <options-plist>)`: Designates an external format
    that is like the one designated by `<keyword>` with options as
    specified in `<options-plist>`.

  Valid options for `<options-plist>` are:

  - `:NEWLINE <newline>`

      An external format with an explicit :NEWLINE option is like its
      `<keyword>` parent but recognizes certain characters or
      character sequences as newlines. For :LF (the default), the
      `#\\Linefeed` character is treated as `#\\Newline` for both
      input and output. For :CR, `#\\Return` is treated as
      `#\\Newline`, while for :CRLF the two-character sequence
      `#\\Return #\\Linefeed` is translated to and from
      `#\\Newline`.

  - `:REPLACEMENT <replacement>`

      An external format with an explicit :REPLACEMENT option is like
      its `<keyword>` parent but does not signal an error in case a
      character or octet sequence cannot be en- or decoded. Instead,
      it inserts `<replacement>` at the position in question.
      `<replacement>` must be a string designator; that is, a
      character or a string.

  For example:

      (with-open-file (stream pathname :external-format '(:utf-8 :replacement #\\?))
        (read-line stream))

  will read the first line of `\\PATHNAME`, replacing any octet
  sequence that is not valid in the UTF-8 external format with a
  question mark character.")

(defsection @character-coding-conditions (:title "Character Coding Conditions")
  "De- or encoding characters using a given external format is not always
  possible:

  - Decoding an octet vector using a given external format can fail if
    it contains an octet or sequence of octets that does not have an
    interpretation as a character according to the external format.

  - Conversely, a string may contain characters that a given external
    format cannot encode. For example, the ASCII external format
    cannot encode the character `#\\ö`.

  Unless the external format governing the coding uses the
  :REPLACEMENT option, SBCL will signal (continuable) errors under the
  above circumstances. The types of the condition signaled are not
  currently exported or documented but will be in future SBCL
  versions.")

(defsection @converting-between-strings-and-octet-vectors
    (:title "Converting between Strings and Octet Vectors")
  "To encode Lisp strings as octet vectors and decode octet vectors as
  Lisp strings, the following SBCL-specific functions can be used:"
  (sb-ext:string-to-octets function)
  (sb-ext:octets-to-string function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun list-external-formats-in-markdown ()
    (flet ((table (items)
             (with-output-to-string (s)
               (loop for (canonical-name . names) in items
                     do (format s "- `~S`~%~%    ~{`~S`~^, ~}~%~%"
                                canonical-name names)))))
      (let (result)
        (loop for ef across sb-impl::*external-formats*
              when (sb-impl::external-format-p ef)
                do
                   (pushnew (sb-impl::ef-names ef) result :test #'equal))
        (table (sort result #'string< :key #'car))))))

(defsection @supported-external-formats (:title "Supported External Formats")
  "The following lists the external formats supported by SBCL in
  the form of the respective canonical name followed by the list of aliases:"
  #.(list-external-formats-in-markdown))
