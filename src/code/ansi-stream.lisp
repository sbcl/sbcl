;;;; the abstract class ANSI-STREAM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; HOW THE ANSI-STREAM STRUCTURE IS USED
;;;
;;; Many of the slots of the ANSI-STREAM structure contain functions
;;; which are called to perform some operation on the stream. Closed
;;; streams have #'CLOSED-FLAME in all of their function slots. If
;;; one side of an I/O or echo stream is closed, the whole stream is
;;; considered closed. The functions in the operation slots take
;;; arguments as follows:
;;;
;;; In:                 Stream, Eof-Errorp, Eof-Value
;;; Bin:                Stream, Eof-Errorp, Eof-Value
;;; N-Bin:              Stream, Buffer, Start, Numbytes, Eof-Errorp
;;; Out:                Stream, Character
;;; Bout:               Stream, Integer
;;; Sout:               Stream, String, Start, End
;;; Misc:               Stream, Operation, &Optional Arg1, Arg2
;;;
;;; In order to save space, some of the less common stream operations
;;; are handled by just one function, the MISC method. This function
;;; is passed a keyword which indicates the operation to perform.
;;; The following keywords are used:
;;;  :listen            - Return the following values:
;;;                          t if any input waiting.
;;;                          :eof if at eof.
;;;                          nil if no input is available and not at eof.
;;;  :unread            - Unread the character Arg.
;;;  :close             - Do any stream specific stuff to close the stream.
;;;                       The methods are set to closed-flame by the close
;;;                       function, so that need not be done by this
;;;                       function.
;;;  :clear-input       - Clear any unread input
;;;  :finish-output,
;;;  :force-output      - Cause output to happen
;;;  :clear-output      - Clear any undone output
;;;  :element-type      - Return the type of element the stream deals with.
;;;  :line-length       - Return the length of a line of output.
;;;  :charpos           - Return current output position on the line.
;;;  :file-length       - Return the file length of a file stream.
;;;  :[gs]et-file-position  - Return or change the current position of a
;;;                       file stream.
;;;  :file-name         - Return the name of an associated file.
;;;  :interactive-p     - Is this an interactive device?
;;;
;;; In order to do almost anything useful, it is necessary to
;;; define a new type of structure that includes stream, so that the
;;; stream can have some state information.
;;;
;;; THE STREAM IN-BUFFER:
;;;
;;; The IN-BUFFER in the stream holds characters or bytes that
;;; are ready to be read by some input function. If there is any
;;; stuff in the IN-BUFFER, then the reading function can use it
;;; without calling any stream method. Any stream may put stuff in
;;; the IN-BUFFER, and may also assume that any input in the IN-BUFFER
;;; has been consumed before any in-method is called. If a text
;;; stream has in IN-BUFFER, then the first character should not be
;;; used to buffer normal input so that it is free for unreading into.
;;;
;;; When the ANSI-STREAM-IN-BUFFER slot, and its index, is only
;;; accessed by the normal stream functions, the number of function
;;; calls is halved, thus potentially doubling the speed of simple
;;; operations. If the FAST-READ-CHAR and FAST-READ-BYTE macros are
;;; used, nearly all function call overhead is removed, vastly
;;; speeding up these important operations.

;;; the size of a stream in-buffer
;;;
;;; This constant it is used in a read-time-eval, and some implementations
;;; draw a sharp distinction between a constant being known only to
;;; the file-compiler during compilation, and known also to the evaluator.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ansi-stream-in-buffer-length+ 512))

(deftype ansi-stream-in-buffer ()
  `(simple-array (unsigned-byte 8) (,+ansi-stream-in-buffer-length+)))

(deftype ansi-stream-cin-buffer ()
  `(simple-array character (,+ansi-stream-in-buffer-length+)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %stream-opcode (name)
  (ecase name
    (:listen              0)
    (:unread              1)
    (:force-output        2)
    (:finish-output       3)
    (:charpos             4)
    (:get-file-position   5)
    (:set-file-position   6)
    (:element-type        7)
    (:element-mode        8)
    (:external-format     9)
    (:line-length        10)
    (:file-length        11)
    (:file-string-length 12)
    (:clear-input        13)
    (:clear-output       14)
    (:close              15)
    (:interactive-p      16)
    ;; This is used by string streams and is not an opcode seen
    ;; by STREAM-MISC-DISPATCH. The new stream pseudomethod convention
    ;; can't pass random keywords to the misc method.
    (:reset-unicode-p    17))))

;;; base class for ANSI standard streams (as opposed to the Gray
;;; streams extension)
(defstruct (ansi-stream (:constructor nil)
                        (:copier nil))

  ;; input buffer
  ;;
  ;; (If a stream does not have an input buffer, then the IN-BUFFER
  ;; slot must must be NIL, and the IN-INDEX must be
  ;; +ANSI-STREAM-IN-BUFFER-LENGTH+.)
  (in-buffer nil :type (or ansi-stream-in-buffer null))
  (cin-buffer nil :type (or ansi-stream-cin-buffer null))
  (in-index +ansi-stream-in-buffer-length+
            :type (integer 0 #.+ansi-stream-in-buffer-length+))

  ;; FIXME: declare all these function types more rigorously.

  ;; buffered input functions
  (in #'ill-in :type function)                  ; READ-CHAR function
  (bin #'ill-bin :type function)                ; byte input function
  ;; 'n-bin' might not transfer bytes to the consumer.
  ;; A character FD-STREAM uses this method to transfer octets from the
  ;; source buffer into characters of the destination buffer.
  (n-bin #'ill-bin :type                        ; n-byte input function
   (sfunction (stream (simple-unboxed-array (*)) index index t) index))

  ;; output functions
  (out #'ill-out :type function)                ; WRITE-CHAR function
  (bout #'ill-bout :type function)              ; byte output function
  (sout #'ill-out :type function)               ; string output function

  ;; other, less-used methods
  (misc #'no-op-placeholder :type (function (stream (integer 0 17) t) *))

  ;; Absolute character position, acting also as a generalized boolean
  ;; in lieu of testing FORM-TRACKING-STREAM-P to see if we must
  ;; maintain correctness of the slot in ANSI-STREAM-UNREAD-CHAR.
  (input-char-pos nil))

;;; SYNONYM-STREAM type is needed by ANSI-STREAM-{INPUT,OUTPUT}-STREAM-P
;;; and also needed by OPEN (though not obviously), which is compiled
;;; prior to some of the stream type definitions in src/code/stream,
;;; so let's define that one here as soon as we can.
(defstruct (synonym-stream (:include ansi-stream
                                     (in #'synonym-in)
                                     (bin #'synonym-bin)
                                     (n-bin #'synonym-n-bin)
                                     (out #'synonym-out)
                                     (bout #'synonym-bout)
                                     (sout #'synonym-sout)
                                     (misc #'synonym-misc))
                           (:constructor make-synonym-stream (symbol))
                           (:copier nil))
  ;; This is the symbol, the value of which is the stream we are synonym to.
  (symbol nil :type symbol :read-only t))
(declaim (freeze-type synonym-stream))

(defstruct (broadcast-stream (:include ansi-stream
                                       (out #'broadcast-out)
                                       (bout #'broadcast-bout)
                                       (sout #'broadcast-sout)
                                       (misc #'broadcast-misc))
                             (:constructor %make-broadcast-stream
                                           (streams))
                             (:copier nil)
                             (:predicate nil))
  ;; a list of all the streams we broadcast to
  (streams () :type list :read-only t))
(declaim (freeze-type broadcast-stream))

(define-load-time-global *null-broadcast-stream* (make-broadcast-stream))
(declaim (type stream *null-broadcast-stream*))

(defmethod print-object ((x stream) stream)
  (print-unreadable-object (x stream :type t :identity t)))

(defmacro with-standard-io-syntax (&body body)
  "Bind the reader and printer control variables to values that enable READ
   to reliably read the results of PRINT. These values are:

         *PACKAGE*                        the COMMON-LISP-USER package
         *PRINT-ARRAY*                    T
         *PRINT-BASE*                     10
         *PRINT-CASE*                     :UPCASE
         *PRINT-CIRCLE*                   NIL
         *PRINT-ESCAPE*                   T
         *PRINT-GENSYM*                   T
         *PRINT-LENGTH*                   NIL
         *PRINT-LEVEL*                    NIL
         *PRINT-LINES*                    NIL
         *PRINT-MISER-WIDTH*              NIL
         *PRINT-PPRINT-DISPATCH*          the standard pprint dispatch table
         *PRINT-PRETTY*                   NIL
         *PRINT-RADIX*                    NIL
         *PRINT-READABLY*                 T
         *PRINT-RIGHT-MARGIN*             NIL
         *READ-BASE*                      10
         *READ-DEFAULT-FLOAT-FORMAT*      SINGLE-FLOAT
         *READ-EVAL*                      T
         *READ-SUPPRESS*                  NIL
         *READTABLE*                      the standard readtable
  SB-EXT:*SUPPRESS-PRINT-ERRORS*          NIL
  SB-EXT:*PRINT-VECTOR-LENGTH*            NIL
"
  (let ((name (make-symbol "THUNK")))
    `(dx-flet ((,name () ,@body))
       (%with-standard-io-syntax #',name))))

;;; Note that this macro may display problems (as do most) with out-of-order
;;; evaluation of keyword args if passed in a different order from listed,
;;; in situations where evaluation order matters.
(defmacro with-input-from-string ((var string &key index
                                                   (start 0 start-suppliedp)
                                                   end)
                                  &body forms-decls
                                  &environment env)
  (let* ((dummy '#:stream) ; in case VAR is declared special
         (offset '#:offset)
         ;; CLHS says in WITH-INPUT-FROM-STRING:
         ;;  "The input string stream is automatically closed on exit from with-input-from-string,
         ;;   no matter whether the exit is normal or abnormal. The input string stream to which
         ;;   the variable var is bound has dynamic extent; its extent ends when the form is exited."
         ;; In light of the second point, we need not close the stream if the object is
         ;; stack-allocated, because any attempt to access it after the forms exit will surely
         ;; crash, and there are otherwise no observable effects from closing the stream.
         ;; The choice to avoid DX-LET in some policies is strictly unecessary, because it is
         ;; *always* undefined behavior to use the DX object after its extent ends,
         ;; however it might help expose user code bugs by keeping the stream accessible
         ;; but closed.
         (bind (if (sb-c:policy env (or (= safety 3) (= debug 3))) 'let 'dx-let))
         (ctor `(%init-string-input-stream
                 ,dummy ,string
                 ;; not (OR START 0), because ":START NIL" should err
                 ,@(cond (start-suppliedp (list start)) (end '(0)))
                 ,@(when end (list end)))))
    (flet ((uwp (forms)
             (if (eq bind 'let)
                 ;; "The consequences are undefined if an attempt is made to assign
                 ;;  the variable var." - so we can read it here with impunity.
                 `(unwind-protect (progn ,@forms) (close ,var))
                 `(progn ,@forms)))) ; don't bother closing
      `(,bind ((,dummy (%make-instance ,(dd-length (find-defstruct-description
                                                    'string-input-stream)))))
              ,(multiple-value-bind (forms decls) (parse-body forms-decls nil)
                 (if index
                     `(multiple-value-bind (,var ,offset) ,ctor
                        ,@decls
                        (multiple-value-prog1 ,(uwp forms)
                          (setf ,index (- (string-input-stream-index ,var) ,offset))))
                     `(let ((,var ,ctor))
                        (declare (ignorable ,var))
                        ,@decls
                        ,(uwp forms)))))))) ; easy way

(defstruct (string-output-stream
            (:include ansi-stream)
            (:constructor nil)
            (:copier nil)
            (:predicate nil))
  ;; Function to perform a piece of the SOUT operation
  ;; Args: (stream buffer string pointer start stop)
  (sout-aux nil :type (sfunction (t t t t t t) t) :read-only t)
  ;; The string we throw stuff in.
  ;; In terms of representation of this buffer, we could do something like
  ;; always use UTF-8, or use a custom encoding that has a bit indicating
  ;; whether the next bits are base-char or character, and then just the
  ;; bits of that character using either 7 bits (filling out 1 byte)
  ;; or 21 bits (consuming 3 or 4 bytes total) on a per-character basis.
  (buffer nil :type (or simple-base-string simple-character-string))
  ;; Whether any non-base character has been written.
  ;; This is :IGNORE for base-char output streams.
  (unicode-p :ignore)
  ;; Chains of buffers to use
  (prev nil :type list)
  (next nil :type list)
  ;; Index of the next location to use in the current string.
  (pointer 0 :type index)
  ;; Global location in the stream
  (index 0 :type index)
  ;; Index cache: when we move backwards we save the greater of this
  ;; and index here, so the greater of index and this is always the
  ;; end of the stream.
  (index-cache 0 :type index)
  ;; Pseudo-actual element type. We no longer store the as-requested type.
  ;; (If the value is :DEFAULT, we return CHARACTER on inquiry.)
  (element-type nil :read-only t
                    :type (member #+sb-unicode :default
                                  #+sb-unicode character
                                  base-char nil)))
(declaim (freeze-type string-output-stream))

(defmacro %allocate-string-ostream ()
  `(%make-instance ,(dd-length (find-defstruct-description 'string-output-stream))))

(defmacro with-output-to-string
    ((var &optional string &key (element-type ''character)) &body body)
  (if string
      (let ((dummy '#:stream))
        ;; "If string is supplied, element-type is ignored".
        ;; Why do implementors take this to mean "evaluated and ignored?"
        ;; I would have figured it meant expansion-time ignored.
        `(dx-let ((,dummy (%make-instance ,(dd-length (find-defstruct-description
                                                       'fill-pointer-output-stream)))))
           (let ((,var (truly-the fill-pointer-output-stream
                                  (%init-fill-pointer-output-stream
                                   ,dummy ,string ,element-type))))
             ;; http://www.lispworks.com/documentation/HyperSpec/Body/d_ignore.htm#ignore
             ;; "The stream variables established by ... with-output-to-string
             ;; are, by definition, always used"
             (declare (ignorable ,var))
             ,@body)))
      (expand-with-output-to-string var element-type body nil)))

;;; Similar to WITH-OUTPUT-TO-STRING, but produces the most compact result
;;; string possible (BASE or CHARACTER) depending on what was written.
;;; This is not something that the standard macro permits.
(defmacro %with-output-to-string ((var) &body body)
  (expand-with-output-to-string var ''character body t))

;;; A macro to better exploit jump tables. Even though jumping based on symbol-hash
;;; is possible, it is of course slightly faster to dispatch on small integers,
;;; and for architectures which don't implement jump tables,
;;; using integers eliminates the load of many code header constants.
;;; Streams which don't want to handle every operation (don't end in a T clause)
;;; should specify :DEFAULT NIL to avoid an error.
(defmacro stream-misc-case ((operation &key (default 'error)) &rest clauses)
  (let* ((otherwise)
         (clauses
          (mapcar (lambda (clause)
                    (let ((key (car clause)))
                      (cons (if (eq key 't)
                                (setq otherwise t)
                                (mapcar #'%stream-opcode (ensure-list (car clause))))
                            (cdr clause))))
                  clauses)))
    `(,(if (and (not otherwise) (eq default 'error)) 'ecase 'case) ,operation
      ,@clauses)))

(defmacro call-ansi-stream-misc (stream operation &optional (arg nil argp))
  ;; Never stuff in a placeholder in the three operations that take an argument.
  (aver (or argp (not (memq operation '(:set-file-position :file-string-length :unread)))))
  `(funcall (ansi-stream-misc ,stream) ,stream
            ;; If operation is a literal keyword, translate it, otherwise
            ;; it is a variable whose values is the opcode for passthru.
            ,(if (keywordp operation) (%stream-opcode operation) operation)
            ,(if argp arg 0)))

;;; This predicate assumes some things:
;;;  - that a simple-stream class will never be FUNCALLABLE-STANDARD-OBJECT
;;;    (so that we need only check for %INSTANCEP)
;;;  - a class will never be redefined such that it moves from one part of the stream
;;;    hierarchy to the other. (i.e. you don't redefine MY-STREAM-CLASS such that
;;;    it was a Gray stream and becomes a simple-stream or vice-versa)
;;;  - instance invalidation, if it needs to happen, will happen somewhere later in
;;;    the stream protocol. (the layout-invalid trap can be deferred)
;;;    This is OK because the next level of dispatch is not to a generic function
;;;    in the simple-stream layering. It is generic at the "device" but not the API.
(defmacro simple-stream-p (x)
  `(and (%instancep ,x)
        (logtest (layout-flags (%instance-layout ,x))
                 sb-kernel::+simple-stream-layout-flag+)))

;;; This macro is for the first-level of dispatch which usually entails
;;; deciding whether the argument is actually a stream or merely a designator
;;; for a stream (T or NIL), and then figuring out which family of stream
;;; it belongs to: ANSI, Gray, or simple.
(defmacro stream-api-dispatch ((streamvar &optional initform) &key native simple gray)
  ;; Most CL: stream APIs use explicit-check, so we should assert that the thing
  ;; is actually a stream.
  ;; If the CL interface bypasses STREAM-API-DISPATCH then it is up to generic layer
  ;; to signal an error, the class of which seems unfortunately subject to debate.
  (aver (and native (or simple gray)))
  `(let ,(if initform `((,streamvar ,initform)))
     ;; Dispatch native first, then if sb-simple-streams have been loaded,
     ;; those, and finally punt to a generic function.
     (block stream
       ;; Assume that simple-stream can not inherit funcallable-standard-object.
       (when (%instancep ,streamvar)
         (let ((layout (%instance-layout ,streamvar)))
           (cond ((sb-c::%structure-is-a layout ,(find-layout 'ansi-stream))
                  (let ((,streamvar (truly-the ansi-stream ,streamvar)))
                    (return-from stream ,native)))
                 ((logtest (layout-flags layout) +simple-stream-layout-flag+)
                  (return-from stream ,simple)))))
       (let ((,streamvar (the stream ,streamvar))) ,gray))))
