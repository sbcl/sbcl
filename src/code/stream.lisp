;;;; os-independent stream functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(deftype string-stream ()
  '(or string-input-stream string-output-stream
       fill-pointer-output-stream))

;;;; standard streams

;;; The initialization of these streams is performed by
;;; STREAM-COLD-INIT-OR-RESET.
(defvar *terminal-io* () #!+sb-doc "Terminal I/O stream.")
(defvar *standard-input* () #!+sb-doc "Default input stream.")
(defvar *standard-output* () #!+sb-doc "Default output stream.")
(defvar *error-output* () #!+sb-doc "Error output stream.")
(defvar *query-io* () #!+sb-doc "Query I/O stream.")
(defvar *trace-output* () #!+sb-doc "Trace output stream.")
(defvar *debug-io* () #!+sb-doc "Interactive debugging stream.")

(defun ill-in (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
	 :datum stream
	 :expected-type '(satisfies input-stream-p)
	 :format-control "~S is not a character input stream."
	 :format-arguments (list stream)))
(defun ill-out (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
	 :datum stream
	 :expected-type '(satisfies output-stream-p)
	 :format-control "~S is not a character output stream."
	 :format-arguments (list stream)))
(defun ill-bin (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
	 :datum stream
	 :expected-type '(satisfies input-stream-p)
	 :format-control "~S is not a binary input stream."
	 :format-arguments (list stream)))
(defun ill-bout (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
	 :datum stream
	 :expected-type '(satisfies output-stream-p)
	 :format-control "~S is not a binary output stream."
	 :format-arguments (list stream)))
(defun closed-flame (stream &rest ignore)
  (declare (ignore ignore))
  (error "~S is closed." stream))
(defun do-nothing (&rest ignore)
  (declare (ignore ignore)))

;;; HOW THE STREAM STRUCTURE IS USED:
;;;
;;; Many of the slots of the stream structure contain functions
;;; which are called to perform some operation on the stream. Closed
;;; streams have #'CLOSED-FLAME in all of their function slots. If
;;; one side of an I/O or echo stream is closed, the whole stream is
;;; considered closed. The functions in the operation slots take
;;; arguments as follows:
;;;
;;; In:			Stream, Eof-Errorp, Eof-Value
;;; Bin:		Stream, Eof-Errorp, Eof-Value
;;; N-Bin:		Stream, Buffer, Start, Numbytes, Eof-Errorp
;;; Out:		Stream, Character
;;; Bout:		Stream, Integer
;;; Sout:		Stream, String, Start, End
;;; Misc:		Stream, Operation, &Optional Arg1, Arg2
;;;
;;; In order to save space, some of the less common stream operations
;;; are handled by just one function, the MISC method. This function
;;; is passed a keyword which indicates the operation to perform.
;;; The following keywords are used:
;;;  :listen 		- Return the following values:
;;; 			     t if any input waiting.
;;; 			     :eof if at eof.
;;; 			     nil if no input is available and not at eof.
;;;  :unread		- Unread the character Arg.
;;;  :close		- Do any stream specific stuff to close the stream.
;;;			  The methods are set to closed-flame by the close
;;;			  function, so that need not be done by this
;;;			  function.
;;;  :clear-input 	- Clear any unread input
;;;  :finish-output,
;;;  :force-output	- Cause output to happen
;;;  :clear-output	- Clear any undone output
;;;  :element-type 	- Return the type of element the stream deals with.
;;;  :line-length	- Return the length of a line of output.
;;;  :charpos		- Return current output position on the line.
;;;  :file-length	- Return the file length of a file stream.
;;;  :file-position	- Return or change the current position of a
;;;                       file stream.
;;;  :file-name		- Return the name of an associated file.
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
;;; The IN-BUFFER slot is a vector +IN-BUFFER-LENGTH+ long. The
;;; IN-INDEX is the index in the IN-BUFFER of the first available
;;; object. The available objects are thus between IN-INDEX and the
;;; length of the IN-BUFFER.
;;;
;;; When this buffer is only accessed by the normal stream
;;; functions, the number of function calls is halved, thus
;;; potentially doubling the speed of simple operations. If the
;;; FAST-READ-CHAR and FAST-READ-BYTE macros are used, nearly all
;;; function call overhead is removed, vastly speeding up these
;;; important operations.
;;;
;;; If a stream does not have an IN-BUFFER, then the IN-BUFFER slot
;;; must be nil, and the IN-INDEX must be +IN-BUFFER-LENGTH+. These are
;;; the default values for the slots.

;;; stream manipulation functions

(defun input-stream-p (stream)
  (declare (type stream stream))

  #!+high-security
  (when (synonym-stream-p stream)
    (setf stream
	  (symbol-value (synonym-stream-symbol stream))))

  (and (lisp-stream-p stream)
       (not (eq (lisp-stream-in stream) #'closed-flame))
       ;;; KLUDGE: It's probably not good to have EQ tests on function
       ;;; values like this. What if someone's redefined the function?
       ;;; Is there a better way? (Perhaps just VALID-FOR-INPUT and
       ;;; VALID-FOR-OUTPUT flags? -- WHN 19990902
       (or (not (eq (lisp-stream-in stream) #'ill-in))
	   (not (eq (lisp-stream-bin stream) #'ill-bin)))))

(defun output-stream-p (stream)
  (declare (type stream stream))

  #!+high-security
  (when (synonym-stream-p stream)
    (setf stream (symbol-value
		  (synonym-stream-symbol stream))))

  (and (lisp-stream-p stream)
       (not (eq (lisp-stream-in stream) #'closed-flame))
       (or (not (eq (lisp-stream-out stream) #'ill-out))
	   (not (eq (lisp-stream-bout stream) #'ill-bout)))))

(defun open-stream-p (stream)
  (declare (type stream stream))
  (not (eq (lisp-stream-in stream) #'closed-flame)))

(defun stream-element-type (stream)
  (declare (type stream stream))
  (funcall (lisp-stream-misc stream) stream :element-type))

(defun interactive-stream-p (stream)
  (declare (type stream stream))
  (funcall (lisp-stream-misc stream) stream :interactive-p))

(defun open-stream-p (stream)
  (declare (type stream stream))
  (not (eq (lisp-stream-in stream) #'closed-flame)))

(defun close (stream &key abort)
  (declare (type stream stream))
  (when (open-stream-p stream)
    (funcall (lisp-stream-misc stream) stream :close abort))
  t)

(defun set-closed-flame (stream)
  (setf (lisp-stream-in stream) #'closed-flame)
  (setf (lisp-stream-bin stream) #'closed-flame)
  (setf (lisp-stream-n-bin stream) #'closed-flame)
  (setf (lisp-stream-in stream) #'closed-flame)
  (setf (lisp-stream-out stream) #'closed-flame)
  (setf (lisp-stream-bout stream) #'closed-flame)
  (setf (lisp-stream-sout stream) #'closed-flame)
  (setf (lisp-stream-misc stream) #'closed-flame))

;;;; file position and file length

;;; Call the MISC method with the :FILE-POSITION operation.
(defun file-position (stream &optional position)
  (declare (type stream stream))
  (declare (type (or index (member nil :start :end)) position))
  (cond
   (position
    (setf (lisp-stream-in-index stream) +in-buffer-length+)
    (funcall (lisp-stream-misc stream) stream :file-position position))
   (t
    (let ((res (funcall (lisp-stream-misc stream) stream :file-position nil)))
      (when res
	(- res (- +in-buffer-length+ (lisp-stream-in-index stream))))))))

;;; This is a literal translation of the ANSI glossary entry "stream
;;; associated with a file".
;;;
;;; KLUDGE: Note that since Unix famously thinks "everything is a
;;; file", and in particular stdin, stdout, and stderr are files, we
;;; end up with this test being satisfied for weird things like
;;; *STANDARD-OUTPUT* (to a tty). That seems unlikely to be what the
;;; ANSI spec really had in mind, especially since this is used as a
;;; qualification for operations like FILE-LENGTH (so that ANSI was
;;; probably thinking of something like what Unix calls block devices)
;;; but I can't see any better way to do it. -- WHN 2001-04-14
(defun stream-associated-with-file-p (x)
  "Test for the ANSI concept \"stream associated with a file\"."
  (or (typep x 'file-stream)
      (and (synonym-stream-p x)
	   (stream-associated-with-file-p (symbol-value
					   (synonym-stream-symbol x))))))

(defun stream-must-be-associated-with-file (stream)
  (declare (type stream stream))
  (unless (stream-associated-with-file-p stream)
    (error 'simple-type-error
	   ;; KLUDGE: The ANSI spec for FILE-LENGTH specifically says
	   ;; this should be TYPE-ERROR. But what then can we use for
	   ;; EXPECTED-TYPE? This SATISFIES type (with a nonstandard
	   ;; private predicate function..) is ugly and confusing, but
	   ;; I can't see any other way. -- WHN 2001-04-14
	   :expected-type '(satisfies stream-associated-with-file-p)
	   :format-string
	   "~@<The stream ~2I~_~S ~I~_isn't associated with a file.~:>"
	   :format-arguments (list stream))))

;;; like FILE-POSITION, only using :FILE-LENGTH
(defun file-length (stream)
  (declare (type (or file-stream synonym-stream) stream))
  (stream-must-be-associated-with-file stream)
  (funcall (lisp-stream-misc stream) stream :file-length))

;;;; input functions

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value
			    recursive-p)
  (declare (ignore recursive-p))
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-char stream
	  (let ((res (make-string 80))
		(len 80)
		(index 0))
	    (loop
	     (let ((ch (fast-read-char nil nil)))
	       (cond (ch
		      (when (char= ch #\newline)
			(done-with-fast-read-char)
			(return (values (shrink-vector res index) nil)))
		      (when (= index len)
			(setq len (* len 2))
			(let ((new (make-string len)))
			  (replace new res)
			  (setq res new)))
		      (setf (schar res index) ch)
		      (incf index))
		     ((zerop index)
		      (done-with-fast-read-char)
		      (return (values (eof-or-lose stream
						   eof-error-p
						   eof-value)
				      t)))
		     ;; Since FAST-READ-CHAR already hit the eof char, we
		     ;; shouldn't do another READ-CHAR.
		     (t
		      (done-with-fast-read-char)
		      (return (values (shrink-vector res index) t))))))))
	;; must be Gray streams FUNDAMENTAL-STREAM
	(multiple-value-bind (string eof) (stream-read-line stream)
	  (if (and eof (zerop (length string)))
	      (values (eof-or-lose stream eof-error-p eof-value) t)
	      (values string eof))))))

;;; We proclaim them INLINE here, then proclaim them MAYBE-INLINE at EOF,
;;; so, except in this file, they are not inline by default, but they can be.
#!-sb-fluid (declaim (inline read-char unread-char read-byte listen))

(defun read-char (&optional (stream *standard-input*)
			    (eof-error-p t)
			    eof-value
			    recursive-p)
  (declare (ignore recursive-p))
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-char stream
	  (prog1
	      (fast-read-char eof-error-p eof-value)
	    (done-with-fast-read-char)))
	;; must be Gray streams FUNDAMENTAL-STREAM
	(let ((char (stream-read-char stream)))
	  (if (eq char :eof)
	      (eof-or-lose stream eof-error-p eof-value)
	      char)))))

(defun unread-char (character &optional (stream *standard-input*))
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(let ((index (1- (lisp-stream-in-index stream)))
	      (buffer (lisp-stream-in-buffer stream)))
	  (declare (fixnum index))
	  (when (minusp index) (error "Nothing to unread."))
	  (cond (buffer
		 (setf (aref buffer index) (char-code character))
		 (setf (lisp-stream-in-index stream) index))
		(t
		 (funcall (lisp-stream-misc stream) stream
			  :unread character))))
	;; must be Gray streams FUNDAMENTAL-STREAM
	(stream-unread-char stream character)))
  nil)

(defun peek-char (&optional (peek-type nil)
			    (stream *standard-input*)
			    (eof-error-p t)
			    eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(let ((char (read-char stream eof-error-p eof-value)))
	  (cond ((eq char eof-value) char)
		((characterp peek-type)
		 (do ((char char (read-char stream eof-error-p eof-value)))
		     ((or (eq char eof-value) (char= char peek-type))
		      (unless (eq char eof-value)
			(unread-char char stream))
		      char)))
		((eq peek-type t)
		 (do ((char char (read-char stream eof-error-p eof-value)))
		     ((or (eq char eof-value) (not (whitespace-char-p char)))
		      (unless (eq char eof-value)
			(unread-char char stream))
		      char)))
		(t
		 (unread-char char stream)
		 char)))
	;; must be Gray streams FUNDAMENTAL-STREAM
	(cond ((characterp peek-type)
	       (do ((char (stream-read-char stream) (stream-read-char stream)))
		   ((or (eq char :eof) (char= char peek-type))
		    (cond ((eq char :eof)
			   (eof-or-lose stream eof-error-p eof-value))
			  (t
			   (stream-unread-char stream char)
			   char)))))
	      ((eq peek-type t)
	       (do ((char (stream-read-char stream) (stream-read-char stream)))
		   ((or (eq char :eof) (not (whitespace-char-p char)))
		    (cond ((eq char :eof)
			   (eof-or-lose stream eof-error-p eof-value))
			  (t
			   (stream-unread-char stream char)
			   char)))))
	      (t
	       (let ((char (stream-peek-char stream)))
		 (if (eq char :eof)
		     (eof-or-lose stream eof-error-p eof-value)
		     char)))))))

(defun listen (&optional (stream *standard-input*))
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(or (/= (the fixnum (lisp-stream-in-index stream)) +in-buffer-length+)
	    ;; Test for T explicitly since misc methods return :EOF sometimes.
	    (eq (funcall (lisp-stream-misc stream) stream :listen) t))
	;; Fall through to Gray streams FUNDAMENTAL-STREAM case.
	(stream-listen stream))))

(defun read-char-no-hang (&optional (stream *standard-input*)
				    (eof-error-p t)
				    eof-value
				    recursive-p)
  (declare (ignore recursive-p))
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(if (funcall (lisp-stream-misc stream) stream :listen)
	    ;; On T or :EOF get READ-CHAR to do the work.
	    (read-char stream eof-error-p eof-value)
	    nil)
	;; must be Gray streams FUNDAMENTAL-STREAM
	(let ((char (stream-read-char-no-hang stream)))
	  (if (eq char :eof)
	      (eof-or-lose stream eof-error-p eof-value)
	      char)))))

(defun clear-input (&optional (stream *standard-input*))
  (let ((stream (in-synonym-of stream)))
    (cond ((lisp-stream-p stream)
	   (setf (lisp-stream-in-index stream) +in-buffer-length+)
	   (funcall (lisp-stream-misc stream) stream :clear-input))
	  (t
	   (stream-clear-input stream))))
  nil)

(declaim (maybe-inline read-byte))
(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((stream (in-synonym-of stream)))
    (if (lisp-stream-p stream)
	(prepare-for-fast-read-byte stream
	  (prog1
	      (fast-read-byte eof-error-p eof-value t)
	    (done-with-fast-read-byte)))
	;; must be Gray streams FUNDAMENTAL-STREAM
	(let ((char (stream-read-byte stream)))
	  (if (eq char :eof)
	      (eof-or-lose stream eof-error-p eof-value)
	      char)))))

;;; Read NUMBYTES bytes into BUFFER beginning at START, and return the
;;; number of bytes read.
;;;
;;; Note: CMU CL's version of this had a special interpretation of
;;; EOF-ERROR-P which SBCL does not have. (In the EOF-ERROR-P=NIL
;;; case, CMU CL's version would return as soon as any data became
;;; available.) This could be useful behavior for things like pipes in
;;; some cases, but it wasn't being used in SBCL, so it was dropped.
;;; If we ever need it, it could be added later as a new variant N-BIN
;;; method (perhaps N-BIN-ASAP?) or something.
(defun read-n-bytes (stream buffer start numbytes &optional (eof-error-p t))
  (declare (type lisp-stream stream)
	   (type index numbytes start)
	   (type (or (simple-array * (*)) system-area-pointer) buffer))
  (let* ((stream (in-synonym-of stream lisp-stream))
	 (in-buffer (lisp-stream-in-buffer stream))
	 (index (lisp-stream-in-index stream))
	 (num-buffered (- +in-buffer-length+ index)))
    (declare (fixnum index num-buffered))
    (cond
     ((not in-buffer)
      (funcall (lisp-stream-n-bin stream)
	       stream
	       buffer
	       start
	       numbytes
	       eof-error-p))
     ((<= numbytes num-buffered)
      (%byte-blt in-buffer index
		 buffer start (+ start numbytes))
      (setf (lisp-stream-in-index stream) (+ index numbytes))
      numbytes)
     (t
      (let ((end (+ start num-buffered)))
	(%byte-blt in-buffer index buffer start end)
	(setf (lisp-stream-in-index stream) +in-buffer-length+)
	(+ (funcall (lisp-stream-n-bin stream)
		    stream
		    buffer
		    end
		    (- numbytes num-buffered)
		    eof-error-p)
	   num-buffered))))))

;;; the amount of space we leave at the start of the in-buffer for
;;; unreading
;;;
;;; (It's 4 instead of 1 to allow word-aligned copies.)
(defconstant +in-buffer-extra+ 4) ; FIXME: should be symbolic constant

;;; This function is called by the FAST-READ-CHAR expansion to refill
;;; the IN-BUFFER for text streams. There is definitely an IN-BUFFER,
;;; and hence must be an N-BIN method.
(defun fast-read-char-refill (stream eof-error-p eof-value)
  (let* ((ibuf (lisp-stream-in-buffer stream))
	 (count (funcall (lisp-stream-n-bin stream)
			 stream
			 ibuf
			 +in-buffer-extra+
			 (- +in-buffer-length+ +in-buffer-extra+)
			 nil))
	 (start (- +in-buffer-length+ count)))
    (declare (type index start count))
    (cond ((zerop count)
	   (setf (lisp-stream-in-index stream) +in-buffer-length+)
	   (funcall (lisp-stream-in stream) stream eof-error-p eof-value))
	  (t
	   (when (/= start +in-buffer-extra+)
	     (bit-bash-copy ibuf (+ (* +in-buffer-extra+ sb!vm:byte-bits)
				    (* sb!vm:vector-data-offset
				       sb!vm:word-bits))
			    ibuf (+ (the index (* start sb!vm:byte-bits))
				    (* sb!vm:vector-data-offset
				       sb!vm:word-bits))
			    (* count sb!vm:byte-bits)))
	   (setf (lisp-stream-in-index stream) (1+ start))
	   (code-char (aref ibuf start))))))

;;; This is similar to FAST-READ-CHAR-REFILL, but we don't have to
;;; leave room for unreading.
(defun fast-read-byte-refill (stream eof-error-p eof-value)
  (let* ((ibuf (lisp-stream-in-buffer stream))
	 (count (funcall (lisp-stream-n-bin stream) stream
			 ibuf 0 +in-buffer-length+
			 nil))
	 (start (- +in-buffer-length+ count)))
    (declare (type index start count))
    (cond ((zerop count)
	   (setf (lisp-stream-in-index stream) +in-buffer-length+)
	   (funcall (lisp-stream-bin stream) stream eof-error-p eof-value))
	  (t
	   (unless (zerop start)
	     (bit-bash-copy ibuf (* sb!vm:vector-data-offset sb!vm:word-bits)
			    ibuf (+ (the index (* start sb!vm:byte-bits))
				    (* sb!vm:vector-data-offset
				       sb!vm:word-bits))
			    (* count sb!vm:byte-bits)))
	   (setf (lisp-stream-in-index stream) (1+ start))
	   (aref ibuf start)))))

;;; output functions

(defun write-char (character &optional (stream *standard-output*))
  (with-out-stream stream (lisp-stream-out character)
		   (stream-write-char character))
  character)

(defun terpri (&optional (stream *standard-output*))
  (with-out-stream stream (lisp-stream-out #\newline) (stream-terpri))
  nil)

(defun fresh-line (&optional (stream *standard-output*))
  (let ((stream (out-synonym-of stream)))
    (if (lisp-stream-p stream)
	(when (/= (or (charpos stream) 1) 0)
	  (funcall (lisp-stream-out stream) stream #\newline)
	  t)
	;; must be Gray streams FUNDAMENTAL-STREAM
	(stream-fresh-line stream))))

(defun write-string (string &optional (stream *standard-output*)
			    &key (start 0) (end (length (the vector string))))

  ;; FIXME: These SETFs don't look right to me. Looking at the
  ;; definition of "bounding indices" in the glossary of the ANSI
  ;; spec, and extrapolating from the behavior of other operations
  ;; when their operands are the wrong type, it seems that it would be
  ;; more correct to essentially
  ;;    (AVER (<= 0 START END (LENGTH STRING)))
  ;; instead of modifying the incorrect values.
  #!+high-security
  (setf end (min end (length (the vector string))))
  #!+high-security
  (setf start (max start 0))

  ;; FIXME: And I'd just signal a non-continuable error..
  #!+high-security
  (when (< end start)
      (cerror "Continue with switched start and end ~S <-> ~S"
	      "Write-string: start (~S) and end (~S) exchanged."
	      start end string)
      (rotatef start end))

  (write-string* string stream start end))

(defun write-string* (string &optional (stream *standard-output*)
			     (start 0) (end (length (the vector string))))
  (declare (fixnum start end))
  (let ((stream (out-synonym-of stream)))
    (cond ((lisp-stream-p stream)
	   (if (array-header-p string)
	       (with-array-data ((data string) (offset-start start)
				 (offset-end end))
		 (funcall (lisp-stream-sout stream)
			  stream data offset-start offset-end))
	       (funcall (lisp-stream-sout stream) stream string start end))
	   string)
	  (t ; must be Gray streams FUNDAMENTAL-STREAM
	   (stream-write-string stream string start end)))))

(defun write-line (string &optional (stream *standard-output*)
			  &key (start 0) (end (length string)))
  (write-line* string stream start end))

(defun write-line* (string &optional (stream *standard-output*)
			   (start 0) (end (length string)))
  (declare (fixnum start end))
  (let ((stream (out-synonym-of stream)))
    (cond ((lisp-stream-p stream)
	   (if (array-header-p string)
	       (with-array-data ((data string) (offset-start start)
				 (offset-end end))
		 (with-out-stream stream (lisp-stream-sout data offset-start
							   offset-end)))
	       (with-out-stream stream (lisp-stream-sout string start end)))
	   (funcall (lisp-stream-out stream) stream #\newline))
	  (t ; must be Gray streams FUNDAMENTAL-STREAM
	   (stream-write-string stream string start end)
	   (stream-write-char stream #\Newline)))
    string))

(defun charpos (&optional (stream *standard-output*))
  (with-out-stream stream (lisp-stream-misc :charpos) (stream-line-column)))

(defun line-length (&optional (stream *standard-output*))
  (with-out-stream stream (lisp-stream-misc :line-length)
		   (stream-line-length)))

(defun finish-output (&optional (stream *standard-output*))
  (with-out-stream stream (lisp-stream-misc :finish-output)
		   (stream-finish-output))
  nil)

(defun force-output (&optional (stream *standard-output*))
  (with-out-stream stream (lisp-stream-misc :force-output)
		   (stream-force-output))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  (with-out-stream stream (lisp-stream-misc :clear-output)
		   (stream-force-output))
  nil)

(defun write-byte (integer stream)
  (with-out-stream stream (lisp-stream-bout integer)
		   (stream-write-byte integer))
  integer)

;;; This is called from LISP-STREAM routines that encapsulate CLOS
;;; streams to handle the misc routines and dispatch to the
;;; appropriate Gray stream functions.
(defun stream-misc-dispatch (stream operation &optional arg1 arg2)
  (declare (type fundamental-stream stream)
	   (ignore arg2))
  (case operation
    (:listen
     ;; Return T if input available, :EOF for end-of-file, otherwise NIL.
     (let ((char (stream-read-char-no-hang stream)))
       (when (characterp char)
	 (stream-unread-char stream char))
       char))
    (:unread
     (stream-unread-char stream arg1))
    (:close
     (close stream))
    (:clear-input
     (stream-clear-input stream))
    (:force-output
     (stream-force-output stream))
    (:finish-output
     (stream-finish-output stream))
    (:element-type
     (stream-element-type stream))
    (:interactive-p
     (interactive-stream-p stream))
    (:line-length
     (stream-line-length stream))
    (:charpos
     (stream-line-column stream))
    (:file-length
     (file-length stream))
    (:file-position
     (file-position stream arg1))))

;;;; broadcast streams

(defstruct (broadcast-stream (:include lisp-stream
				       (out #'broadcast-out)
				       (bout #'broadcast-bout)
				       (sout #'broadcast-sout)
				       (misc #'broadcast-misc))
			     (:constructor #!-high-security-support
					   make-broadcast-stream
					   #!+high-security-support
					   %make-broadcast-stream (&rest
								   streams))
			     (:copier nil))
  ;; a list of all the streams we broadcast to
  (streams () :type list :read-only t))

#!+high-security-support
(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (unless (or (and (synonym-stream-p stream)
		     (output-stream-p (symbol-value
				       (synonym-stream-symbol stream))))
		(output-stream-p stream))
      (error 'type-error
	     :datum stream
	     :expected-type '(satisfies output-stream-p))))
  (apply #'%make-broadcast-stream streams))

(macrolet ((out-fun (fun method stream-method &rest args)
	     `(defun ,fun (stream ,@args)
		(dolist (stream (broadcast-stream-streams stream))
		  (if (lisp-stream-p stream)
		      (funcall (,method stream) stream ,@args)
		      (,stream-method stream ,@args))))))
  (out-fun broadcast-out lisp-stream-out stream-write-char char)
  (out-fun broadcast-bout lisp-stream-bout stream-write-byte byte)
  (out-fun broadcast-sout lisp-stream-sout stream-write-string
	   string start end))

(defun broadcast-misc (stream operation &optional arg1 arg2)
  (let ((streams (broadcast-stream-streams stream)))
    (case operation
      (:charpos
       (dolist (stream streams)
	 (let ((charpos (charpos stream)))
	   (if charpos (return charpos)))))
      (:line-length
       (let ((min nil))
	 (dolist (stream streams min)
	   (let ((res (line-length stream)))
	     (when res (setq min (if min (min res min) res)))))))
      (:element-type
       (let (res)
	 (dolist (stream streams (if (> (length res) 1) `(and ,@res) res))
	   (pushnew (stream-element-type stream) res :test #'equal))))
      (:close)
      (t
       (let ((res nil))
	 (dolist (stream streams res)
	   (setq res
		 (if (lisp-stream-p stream)
		     (funcall (lisp-stream-misc stream) stream operation
			      arg1 arg2)
		     (stream-misc-dispatch stream operation arg1 arg2)))))))))

;;;; synonym streams

(defstruct (synonym-stream (:include lisp-stream
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
(def!method print-object ((x synonym-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":SYMBOL ~S" (synonym-stream-symbol x))))

;;; The output simple output methods just call the corresponding method
;;; in the synonymed stream.
(macrolet ((out-fun (name slot stream-method &rest args)
	     `(defun ,name (stream ,@args)
		(declare (optimize (safety 1)))
		(let ((syn (symbol-value (synonym-stream-symbol stream))))
		  (if (lisp-stream-p syn)
		      (funcall (,slot syn) syn ,@args)
		      (,stream-method syn ,@args))))))
  (out-fun synonym-out lisp-stream-out stream-write-char ch)
  (out-fun synonym-bout lisp-stream-bout stream-write-byte n)
  (out-fun synonym-sout lisp-stream-sout stream-write-string string start end))

;;; For the input methods, we just call the corresponding function on the
;;; synonymed stream. These functions deal with getting input out of
;;; the In-Buffer if there is any.
(macrolet ((in-fun (name fun &rest args)
	     `(defun ,name (stream ,@args)
		(declare (optimize (safety 1)))
		(,fun (symbol-value (synonym-stream-symbol stream))
		      ,@args))))
  (in-fun synonym-in read-char eof-error-p eof-value)
  (in-fun synonym-bin read-byte eof-error-p eof-value)
  (in-fun synonym-n-bin read-n-bytes buffer start numbytes eof-error-p))

(defun synonym-misc (stream operation &optional arg1 arg2)
  (declare (optimize (safety 1)))
  (let ((syn (symbol-value (synonym-stream-symbol stream))))
    (if (lisp-stream-p syn)
	;; We have to special-case some operations which interact with
	;; the in-buffer of the wrapped stream, since just calling
	;; LISP-STREAM-MISC on them
 	(case operation
	  (:listen (or (/= (the fixnum (lisp-stream-in-index syn))
			   +in-buffer-length+)
		       (funcall (lisp-stream-misc syn) syn :listen)))
          (:clear-input (clear-input syn))
          (:unread (unread-char arg1 syn))
	  (t
	   (funcall (lisp-stream-misc syn) syn operation arg1 arg2)))
	(stream-misc-dispatch syn operation arg1 arg2))))

;;;; two-way streams

(defstruct (two-way-stream
	    (:include lisp-stream
		      (in #'two-way-in)
		      (bin #'two-way-bin)
		      (n-bin #'two-way-n-bin)
		      (out #'two-way-out)
		      (bout #'two-way-bout)
		      (sout #'two-way-sout)
		      (misc #'two-way-misc))
	    (:constructor #!-high-security-support
			  make-two-way-stream
			  #!+high-security-support
			  %make-two-way-stream (input-stream output-stream))
	    (:copier nil))
  (input-stream (required-argument) :type stream :read-only t)
  (output-stream (required-argument) :type stream :read-only t))
(defprinter (two-way-stream) input-stream output-stream)

#!-high-security-support
(setf (fdocumentation 'make-two-way-stream 'function)
  "Return a bidirectional stream which gets its input from Input-Stream and
   sends its output to Output-Stream.")
#!+high-security-support
(defun make-two-way-stream (input-stream output-stream)
  #!+sb-doc
  "Return a bidirectional stream which gets its input from Input-Stream and
   sends its output to Output-Stream."
  ;; FIXME: This idiom of the-real-stream-of-a-possibly-synonym-stream
  ;; should be encapsulated in a function, and used here and most of
  ;; the other places that SYNONYM-STREAM-P appears.
  (unless (or (and (synonym-stream-p output-stream)
	 	   (output-stream-p (symbol-value
				     (synonym-stream-symbol output-stream))))
	      (output-stream-p output-stream))
    (error 'type-error
	   :datum output-stream
	   :expected-type '(satisfies output-stream-p)))
  (unless (or (and (synonym-stream-p input-stream)
		   (input-stream-p (symbol-value
				    (synonym-stream-symbol input-stream))))
	      (input-stream-p input-stream))
    (error 'type-error
	   :datum input-stream
	   :expected-type '(satisfies input-stream-p)))
  (funcall #'%make-two-way-stream input-stream output-stream))

(macrolet ((out-fun (name slot stream-method &rest args)
	     `(defun ,name (stream ,@args)
		(let ((syn (two-way-stream-output-stream stream)))
		  (if (lisp-stream-p syn)
		      (funcall (,slot syn) syn ,@args)
		      (,stream-method syn ,@args))))))
  (out-fun two-way-out lisp-stream-out stream-write-char ch)
  (out-fun two-way-bout lisp-stream-bout stream-write-byte n)
  (out-fun two-way-sout lisp-stream-sout stream-write-string string start end))

(macrolet ((in-fun (name fun &rest args)
	     `(defun ,name (stream ,@args)
		(force-output (two-way-stream-output-stream stream))
		(,fun (two-way-stream-input-stream stream) ,@args))))
  (in-fun two-way-in read-char eof-error-p eof-value)
  (in-fun two-way-bin read-byte eof-error-p eof-value)
  (in-fun two-way-n-bin read-n-bytes buffer start numbytes eof-error-p))

(defun two-way-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
	 (out (two-way-stream-output-stream stream))
	 (in-lisp-stream-p (lisp-stream-p in))
	 (out-lisp-stream-p (lisp-stream-p out)))
    (case operation
      (:listen
       (if in-lisp-stream-p
	   (or (/= (the fixnum (lisp-stream-in-index in)) +in-buffer-length+)
	       (funcall (lisp-stream-misc in) in :listen))
	   (stream-listen in)))
      ((:finish-output :force-output :clear-output)
       (if out-lisp-stream-p
	   (funcall (lisp-stream-misc out) out operation arg1 arg2)
	   (stream-misc-dispatch out operation arg1 arg2)))
      (:clear-input (clear-input in))
      (:unread (unread-char arg1 in))
      (:element-type
       (let ((in-type (stream-element-type in))
	     (out-type (stream-element-type out)))
	 (if (equal in-type out-type)
	     in-type `(and ,in-type ,out-type))))
      (:close
       (set-closed-flame stream))
      (t
       (or (if in-lisp-stream-p
	       (funcall (lisp-stream-misc in) in operation arg1 arg2)
	       (stream-misc-dispatch in operation arg1 arg2))
	   (if out-lisp-stream-p
	       (funcall (lisp-stream-misc out) out operation arg1 arg2)
	       (stream-misc-dispatch out operation arg1 arg2)))))))

;;;; concatenated streams

(defstruct (concatenated-stream
	    (:include lisp-stream
		      (in #'concatenated-in)
		      (bin #'concatenated-bin)
		      (n-bin #'concatenated-n-bin)
		      (misc #'concatenated-misc))
	    (:constructor
	     #!-high-security-support make-concatenated-stream
	     #!+high-security-support %make-concatenated-stream
		 (&rest streams &aux (current streams)))
	    (:copier nil))
  ;; The car of this is the substream we are reading from now.
  current
  ;; This is a list of all the substreams there ever were. We need to
  ;; remember them so that we can close them.
  ;;
  ;; FIXME: ANSI says this is supposed to be the list of streams that
  ;; we still have to read from. So either this needs to become a
  ;; private member %STREAM (with CONCATENATED-STREAM-STREAMS a wrapper
  ;; around it which discards closed files from the head of the list)
  ;; or we need to update it as we run out of files.
  (streams nil :type list :read-only t))
(def!method print-object ((x concatenated-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream
	    ":STREAMS ~S"
	    (concatenated-stream-streams x))))

#!-high-security-support
(setf (fdocumentation 'make-concatenated-stream 'function)
  "Returns a stream which takes its input from each of the Streams in turn,
   going on to the next at EOF.")

#!+high-security-support
(defun make-concatenated-stream (&rest streams)
  #!+sb-doc
  "Returns a stream which takes its input from each of the Streams in turn,
   going on to the next at EOF."
  (dolist (stream streams)
    (unless (or (and (synonym-stream-p stream)
		     (input-stream-p (symbol-value
				      (synonym-stream-symbol stream))))
		(input-stream-p stream))
      (error 'type-error
	     :datum stream
	     :expected-type '(satisfies input-stream-p))))
  (apply #'%make-concatenated-stream streams))

(macrolet ((in-fun (name fun)
	     `(defun ,name (stream eof-error-p eof-value)
		(do ((current (concatenated-stream-current stream)
			      (cdr current)))
		    ((null current)
		     (eof-or-lose stream eof-error-p eof-value))
		  (let* ((stream (car current))
			 (result (,fun stream nil nil)))
		    (when result (return result)))
		  (setf (concatenated-stream-current stream) current)))))
  (in-fun concatenated-in read-char)
  (in-fun concatenated-bin read-byte))

(defun concatenated-n-bin (stream buffer start numbytes eof-errorp)
  (do ((current (concatenated-stream-current stream) (cdr current))
       (current-start start)
       (remaining-bytes numbytes))
      ((null current)
       (if eof-errorp
	   (error 'end-of-file :stream stream)
	   (- numbytes remaining-bytes)))
    (let* ((stream (car current))
           (bytes-read (read-n-bytes stream buffer current-start
	                             remaining-bytes nil)))
      (incf current-start bytes-read)
      (decf remaining-bytes bytes-read)
      (when (zerop remaining-bytes) (return numbytes)))
    (setf (concatenated-stream-current stream) (cdr current))))

(defun concatenated-misc (stream operation &optional arg1 arg2)
  (let ((left (concatenated-stream-current stream)))
    (when left
      (let* ((current (car left)))
	(case operation
	  (:listen
	   (loop
	     (let ((stuff (if (lisp-stream-p current)
			      (funcall (lisp-stream-misc current) current
				       :listen)
			      (stream-misc-dispatch current :listen))))
	       (cond ((eq stuff :eof)
		      ;; Advance CURRENT, and try again.
		      (pop (concatenated-stream-current stream))
		      (setf current
			    (car (concatenated-stream-current stream)))
		      (unless current
			;; No further streams. EOF.
			(return :eof)))
		     (stuff
		      ;; Stuff's available.
		      (return t))
		     (t
		      ;; Nothing is available yet.
		      (return nil))))))
          (:clear-input (clear-input current))
          (:unread (unread-char arg1 current))
          (:close
	   (set-closed-flame stream))
	  (t
	   (if (lisp-stream-p current)
	       (funcall (lisp-stream-misc current) current operation arg1 arg2)
	       (stream-misc-dispatch current operation arg1 arg2))))))))

;;;; echo streams

(defstruct (echo-stream
	    (:include two-way-stream
		      (in #'echo-in)
		      (bin #'echo-bin)
		      (misc #'echo-misc)
		      (n-bin #'ill-bin))
	    (:constructor make-echo-stream (input-stream output-stream))
	    (:copier nil))
  unread-stuff)
(def!method print-object ((x echo-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream
	    ":INPUT-STREAM ~S :OUTPUT-STREAM ~S"
	    (two-way-stream-input-stream x)
	    (two-way-stream-output-stream x))))

(macrolet ((in-fun (name fun out-slot stream-method &rest args)
	     `(defun ,name (stream ,@args)
		(or (pop (echo-stream-unread-stuff stream))
		    (let* ((in (echo-stream-input-stream stream))
			   (out (echo-stream-output-stream stream))
			   (result (,fun in ,@args)))
		      (if (lisp-stream-p out)
			  (funcall (,out-slot out) out result)
			  (,stream-method out result))
		      result)))))
  (in-fun echo-in read-char lisp-stream-out stream-write-char
	  eof-error-p eof-value)
  (in-fun echo-bin read-byte lisp-stream-bout stream-write-byte
	  eof-error-p eof-value))

(defun echo-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
	 (out (two-way-stream-output-stream stream)))
    (case operation
      (:listen
       (or (not (null (echo-stream-unread-stuff stream)))
	   (if (lisp-stream-p in)
	       (or (/= (the fixnum (lisp-stream-in-index in))
		       +in-buffer-length+)
		   (funcall (lisp-stream-misc in) in :listen))
	       (stream-misc-dispatch in :listen))))
      (:unread (push arg1 (echo-stream-unread-stuff stream)))
      (:element-type
       (let ((in-type (stream-element-type in))
	     (out-type (stream-element-type out)))
	 (if (equal in-type out-type)
	     in-type `(and ,in-type ,out-type))))
      (:close
       (set-closed-flame stream))
      (t
       (or (if (lisp-stream-p in)
	       (funcall (lisp-stream-misc in) in operation arg1 arg2)
	       (stream-misc-dispatch in operation arg1 arg2))
	   (if (lisp-stream-p out)
	       (funcall (lisp-stream-misc out) out operation arg1 arg2)
	       (stream-misc-dispatch out operation arg1 arg2)))))))

#!+sb-doc
(setf (fdocumentation 'make-echo-stream 'function)
  "Returns a bidirectional stream which gets its input from Input-Stream and
   sends its output to Output-Stream. In addition, all input is echoed to
   the output stream")

;;;; string input streams

(defstruct (string-input-stream
	     (:include lisp-stream
		       (in #'string-inch)
		       (bin #'string-binch)
		       (n-bin #'string-stream-read-n-bytes)
		       (misc #'string-in-misc))
	     (:constructor internal-make-string-input-stream
			   (string current end))
	     (:copier nil))
  (string nil :type simple-string)
  (current nil :type index)
  (end nil :type index))

(defun string-inch (stream eof-error-p eof-value)
  (let ((string (string-input-stream-string stream))
	(index (string-input-stream-current stream)))
    (declare (simple-string string) (fixnum index))
    (cond ((= index (the index (string-input-stream-end stream)))
	   (eof-or-lose stream eof-error-p eof-value))
	  (t
	   (setf (string-input-stream-current stream) (1+ index))
	   (aref string index)))))

(defun string-binch (stream eof-error-p eof-value)
  (let ((string (string-input-stream-string stream))
	(index (string-input-stream-current stream)))
    (declare (simple-string string)
	     (type index index))
    (cond ((= index (the index (string-input-stream-end stream)))
	   (eof-or-lose stream eof-error-p eof-value))
	  (t
	   (setf (string-input-stream-current stream) (1+ index))
	   (char-code (aref string index))))))

(defun string-stream-read-n-bytes (stream buffer start requested eof-error-p)
  (declare (type string-input-stream stream)
	   (type index start requested))
  (let* ((string (string-input-stream-string stream))
	 (index (string-input-stream-current stream))
	 (available (- (string-input-stream-end stream) index))
	 (copy (min available requested)))
    (declare (simple-string string)
	     (type index index available copy))
    (when (plusp copy)
      (setf (string-input-stream-current stream)
	    (truly-the index (+ index copy)))
      (sb!sys:without-gcing
       (system-area-copy (vector-sap string)
			 (* index sb!vm:byte-bits)
			 (if (typep buffer 'system-area-pointer)
			     buffer
			     (vector-sap buffer))
			 (* start sb!vm:byte-bits)
			 (* copy sb!vm:byte-bits))))
    (if (and (> requested copy) eof-error-p)
	(error 'end-of-file :stream stream)
	copy)))

(defun string-in-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (if arg1
	 (setf (string-input-stream-current stream) arg1)
	 (string-input-stream-current stream)))
    (:file-length (length (string-input-stream-string stream)))
    (:unread (decf (string-input-stream-current stream)))
    (:listen (or (/= (the fixnum (string-input-stream-current stream))
		     (the fixnum (string-input-stream-end stream)))
		 :eof))
    (:element-type 'base-char)))

(defun make-string-input-stream (string &optional
					(start 0) (end (length string)))
  #!+sb-doc
  "Returns an input stream which will supply the characters of String between
  Start and End in order."
  (declare (type string string)
	   (type index start)
	   (type (or index null) end))

  #!+high-security
  (when (> end (length string))
    (cerror "Continue with end changed from ~S to ~S"
	    "Write-string: end (~S) is larger then the length of the string (~S)"
	    end (1- (length string))))

  (internal-make-string-input-stream (coerce string 'simple-string)
				     start end))

;;;; string output streams

(defstruct (string-output-stream
	    (:include lisp-stream
		      (out #'string-ouch)
		      (sout #'string-sout)
		      (misc #'string-out-misc))
	    (:constructor make-string-output-stream ())
	    (:copier nil))
  ;; The string we throw stuff in.
  (string (make-string 40) :type simple-string)
  ;; Index of the next location to use.
  (index 0 :type fixnum))

#!+sb-doc
(setf (fdocumentation 'make-string-output-stream 'function)
  "Returns an Output stream which will accumulate all output given it for
   the benefit of the function Get-Output-Stream-String.")

(defun string-ouch (stream character)
  (let ((current (string-output-stream-index stream))
	(workspace (string-output-stream-string stream)))
    (declare (simple-string workspace) (fixnum current))
    (if (= current (the fixnum (length workspace)))
	(let ((new-workspace (make-string (* current 2))))
	  (replace new-workspace workspace)
	  (setf (aref new-workspace current) character)
	  (setf (string-output-stream-string stream) new-workspace))
	(setf (aref workspace current) character))
    (setf (string-output-stream-index stream) (1+ current))))

(defun string-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (let* ((current (string-output-stream-index stream))
	 (length (- end start))
	 (dst-end (+ length current))
	 (workspace (string-output-stream-string stream)))
    (declare (simple-string workspace)
	     (fixnum current length dst-end))
    (if (> dst-end (the fixnum (length workspace)))
	(let ((new-workspace (make-string (+ (* current 2) length))))
	  (replace new-workspace workspace :end2 current)
	  (replace new-workspace string
		   :start1 current :end1 dst-end
		   :start2 start :end2 end)
	  (setf (string-output-stream-string stream) new-workspace))
	(replace workspace string
		 :start1 current :end1 dst-end
		 :start2 start :end2 end))
    (setf (string-output-stream-index stream) dst-end)))

(defun string-out-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (if (null arg1)
	 (string-output-stream-index stream)))
    (:charpos
     (do ((index (1- (the fixnum (string-output-stream-index stream)))
		 (1- index))
	  (count 0 (1+ count))
	  (string (string-output-stream-string stream)))
	 ((< index 0) count)
       (declare (simple-string string)
		(fixnum index count))
       (if (char= (schar string index) #\newline)
	   (return count))))
    (:element-type 'base-char)))

;;; Return a string of all the characters sent to a stream made by
;;; MAKE-STRING-OUTPUT-STREAM since the last call to this function.
(defun get-output-stream-string (stream)
  (declare (type string-output-stream stream))
  (let* ((length (string-output-stream-index stream))
	 (result (make-string length)))
    (replace result (string-output-stream-string stream))
    (setf (string-output-stream-index stream) 0)
    result))

;;; Dump the characters buffer up in IN-STREAM to OUT-STREAM as
;;; GET-OUTPUT-STREAM-STRING would return them.
(defun dump-output-stream-string (in-stream out-stream)
  (write-string* (string-output-stream-string in-stream) out-stream
		 0 (string-output-stream-index in-stream))
  (setf (string-output-stream-index in-stream) 0))

;;;; fill-pointer streams

;;; Fill pointer STRING-OUTPUT-STREAMs are not explicitly mentioned in
;;; the CLM, but they are required for the implementation of
;;; WITH-OUTPUT-TO-STRING.

(defstruct (fill-pointer-output-stream
 	    (:include lisp-stream
		      (out #'fill-pointer-ouch)
		      (sout #'fill-pointer-sout)
		      (misc #'fill-pointer-misc))
	    (:constructor make-fill-pointer-output-stream (string))
	    (:copier nil))
  ;; the string we throw stuff in
  string)

(defun fill-pointer-ouch (stream character)
  (let* ((buffer (fill-pointer-output-stream-string stream))
	 (current (fill-pointer buffer))
	 (current+1 (1+ current)))
    (declare (fixnum current))
    (with-array-data ((workspace buffer) (start) (end))
      (declare (simple-string workspace))
      (let ((offset-current (+ start current)))
	(declare (fixnum offset-current))
	(if (= offset-current end)
	    (let* ((new-length (* current 2))
		   (new-workspace (make-string new-length)))
	      (declare (simple-string new-workspace))
	      (%byte-blt workspace start
			 new-workspace 0 current)
	      (setf workspace new-workspace)
	      (setf offset-current current)
	      (set-array-header buffer workspace new-length
				current+1 0 new-length nil))
	    (setf (fill-pointer buffer) current+1))
	(setf (schar workspace offset-current) character)))
    current+1))

(defun fill-pointer-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (let* ((buffer (fill-pointer-output-stream-string stream))
	 (current (fill-pointer buffer))
	 (string-len (- end start))
	 (dst-end (+ string-len current)))
    (declare (fixnum current dst-end string-len))
    (with-array-data ((workspace buffer) (dst-start) (dst-length))
      (declare (simple-string workspace))
      (let ((offset-dst-end (+ dst-start dst-end))
	    (offset-current (+ dst-start current)))
	(declare (fixnum offset-dst-end offset-current))
	(if (> offset-dst-end dst-length)
	    (let* ((new-length (+ (the fixnum (* current 2)) string-len))
		   (new-workspace (make-string new-length)))
	      (declare (simple-string new-workspace))
	      (%byte-blt workspace dst-start
			 new-workspace 0 current)
	      (setf workspace new-workspace)
	      (setf offset-current current)
	      (setf offset-dst-end dst-end)
	      (set-array-header buffer
				workspace
				new-length
				dst-end
				0
				new-length
				nil))
	    (setf (fill-pointer buffer) dst-end))
	(%byte-blt string start
		   workspace offset-current offset-dst-end)))
    dst-end))

(defun fill-pointer-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (case operation
    (:charpos
     (let* ((buffer (fill-pointer-output-stream-string stream))
	    (current (fill-pointer buffer)))
       (with-array-data ((string buffer) (start) (end current))
	 (declare (simple-string string) (ignore start))
	 (let ((found (position #\newline string :test #'char=
				:end end :from-end t)))
	   (if found
	       (- end (the fixnum found))
	       current)))))
     (:element-type 'base-char)))

;;;; indenting streams

(defstruct (indenting-stream (:include lisp-stream
				       (out #'indenting-out)
				       (sout #'indenting-sout)
				       (misc #'indenting-misc))
			     (:constructor make-indenting-stream (stream))
			     (:copier nil))
  ;; the stream we're based on
  stream
  ;; how much we indent on each line
  (indentation 0))

#!+sb-doc
(setf (fdocumentation 'make-indenting-stream 'function)
 "Returns an output stream which indents its output by some amount.")

;;; INDENTING-INDENT writes the correct number of spaces needed to indent
;;; output on the given STREAM based on the specified SUB-STREAM.
(defmacro indenting-indent (stream sub-stream)
  ;; KLUDGE: bare magic number 60
  `(do ((i 0 (+ i 60))
	(indentation (indenting-stream-indentation ,stream)))
       ((>= i indentation))
     (write-string*
      "							    "
      ,sub-stream 0 (min 60 (- indentation i)))))

;;; INDENTING-OUT writes a character to an indenting stream.
(defun indenting-out (stream char)
  (let ((sub-stream (indenting-stream-stream stream)))
    (write-char char sub-stream)
    (if (char= char #\newline)
	(indenting-indent stream sub-stream))))

;;; INDENTING-SOUT writes a string to an indenting stream.
(defun indenting-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (do ((i start)
       (sub-stream (indenting-stream-stream stream)))
      ((= i end))
    (let ((newline (position #\newline string :start i :end end)))
      (cond (newline
	     (write-string* string sub-stream i (1+ newline))
	     (indenting-indent stream sub-stream)
	     (setq i (+ newline 1)))
	    (t
	     (write-string* string sub-stream i end)
	     (setq i end))))))

;;; INDENTING-MISC just treats just the :LINE-LENGTH message
;;; differently. INDENTING-CHARPOS says the charpos is the charpos of
;;; the base stream minus the stream's indentation.
(defun indenting-misc (stream operation &optional arg1 arg2)
  (let ((sub-stream (indenting-stream-stream stream)))
    (if (lisp-stream-p sub-stream)
	(let ((method (lisp-stream-misc sub-stream)))
	  (case operation
	    (:line-length
	     (let ((line-length (funcall method sub-stream operation)))
	       (if line-length
		   (- line-length (indenting-stream-indentation stream)))))
	    (:charpos
	     (let ((charpos (funcall method sub-stream operation)))
	       (if charpos
		   (- charpos (indenting-stream-indentation stream)))))
	    (t
	     (funcall method sub-stream operation arg1 arg2))))
	;; must be Gray streams FUNDAMENTAL-STREAM
	(case operation
	  (:line-length
	   (let ((line-length (stream-line-length sub-stream)))
	     (if line-length
		 (- line-length (indenting-stream-indentation stream)))))
	  (:charpos
	   (let ((charpos (stream-line-column sub-stream)))
	     (if charpos
		 (- charpos (indenting-stream-indentation stream)))))
	  (t
	   (stream-misc-dispatch sub-stream operation arg1 arg2))))))

(declaim (maybe-inline read-char unread-char read-byte listen))

;;;; case frobbing streams, used by FORMAT ~(...~)

(defstruct (case-frob-stream
	    (:include lisp-stream
		      (:misc #'case-frob-misc))
	    (:constructor %make-case-frob-stream (target out sout))
	    (:copier nil))
  (target (required-argument) :type stream))

(defun make-case-frob-stream (target kind)
  #!+sb-doc
  "Returns a stream that sends all output to the stream TARGET, but modifies
   the case of letters, depending on KIND, which should be one of:
     :upcase - convert to upper case.
     :downcase - convert to lower case.
     :capitalize - convert the first letter of words to upper case and the
	rest of the word to lower case.
     :capitalize-first - convert the first letter of the first word to upper
	case and everything else to lower case."
  (declare (type stream target)
	   (type (member :upcase :downcase :capitalize :capitalize-first)
		 kind)
	   (values stream))
  (if (case-frob-stream-p target)
      ;; If we are going to be writing to a stream that already does
      ;; case frobbing, why bother frobbing the case just so it can
      ;; frob it again?
      target
      (multiple-value-bind (out sout)
	  (ecase kind
	    (:upcase
	     (values #'case-frob-upcase-out
		     #'case-frob-upcase-sout))
	    (:downcase
	     (values #'case-frob-downcase-out
		     #'case-frob-downcase-sout))
	    (:capitalize
	     (values #'case-frob-capitalize-out
		     #'case-frob-capitalize-sout))
	    (:capitalize-first
	     (values #'case-frob-capitalize-first-out
		     #'case-frob-capitalize-first-sout)))
	(%make-case-frob-stream target out sout))))

(defun case-frob-misc (stream op &optional arg1 arg2)
  (declare (type case-frob-stream stream))
  (case op
    (:close)
    (t
     (let ((target (case-frob-stream-target stream)))
       (if (lisp-stream-p target)
	   (funcall (lisp-stream-misc target) target op arg1 arg2)
	   (stream-misc-dispatch target op arg1 arg2))))))

(defun case-frob-upcase-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream))
	(char (char-upcase char)))
    (if (lisp-stream-p target)
	(funcall (lisp-stream-out target) target char)
	(stream-write-char target char))))

(defun case-frob-upcase-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (len (length str))
	 (end (or end len))
	 (string (if (and (zerop start) (= len end))
		     (string-upcase str)
		     (nstring-upcase (subseq str start end))))
	 (string-len (- end start)))
    (if (lisp-stream-p target)
	(funcall (lisp-stream-sout target) target string 0 string-len)
	(stream-write-string target string 0 string-len))))

(defun case-frob-downcase-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream))
	(char (char-downcase char)))
    (if (lisp-stream-p target)
	(funcall (lisp-stream-out target) target char)
	(stream-write-char target char))))

(defun case-frob-downcase-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (len (length str))
	 (end (or end len))
	 (string (if (and (zerop start) (= len end))
		     (string-downcase str)
		     (nstring-downcase (subseq str start end))))
	 (string-len (- end start)))
    (if (lisp-stream-p target)
	(funcall (lisp-stream-sout target) target string 0 string-len)
	(stream-write-string target string 0 string-len))))

(defun case-frob-capitalize-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (let ((char (char-upcase char)))
	     (if (lisp-stream-p target)
		 (funcall (lisp-stream-out target) target char)
		 (stream-write-char target char)))
	   (setf (case-frob-stream-out stream) #'case-frob-capitalize-aux-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-capitalize-aux-sout))
	  (t
	   (if (lisp-stream-p target)
	       (funcall (lisp-stream-out target) target char)
	       (stream-write-char target char))))))

(defun case-frob-capitalize-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (str (subseq str start end))
	 (len (length str))
	 (inside-word nil))
    (dotimes (i len)
      (let ((char (schar str i)))
	(cond ((not (alphanumericp char))
	       (setf inside-word nil))
	      (inside-word
	       (setf (schar str i) (char-downcase char)))
	      (t
	       (setf inside-word t)
	       (setf (schar str i) (char-upcase char))))))
    (when inside-word
      (setf (case-frob-stream-out stream)
	    #'case-frob-capitalize-aux-out)
      (setf (case-frob-stream-sout stream)
	    #'case-frob-capitalize-aux-sout))
    (if (lisp-stream-p target)
	(funcall (lisp-stream-sout target) target str 0 len)
	(stream-write-string target str 0 len))))

(defun case-frob-capitalize-aux-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (let ((char (char-downcase char)))
	     (if (lisp-stream-p target)
		 (funcall (lisp-stream-out target) target char)
		 (stream-write-char target char))))
	  (t
	   (if (lisp-stream-p target)
	       (funcall (lisp-stream-out target) target char)
	       (stream-write-char target char))
	   (setf (case-frob-stream-out stream)
		 #'case-frob-capitalize-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-capitalize-sout)))))

(defun case-frob-capitalize-aux-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (str (subseq str start end))
	 (len (length str))
	 (inside-word t))
    (dotimes (i len)
      (let ((char (schar str i)))
	(cond ((not (alphanumericp char))
	       (setf inside-word nil))
	      (inside-word
	       (setf (schar str i) (char-downcase char)))
	      (t
	       (setf inside-word t)
	       (setf (schar str i) (char-upcase char))))))
    (unless inside-word
      (setf (case-frob-stream-out stream)
	    #'case-frob-capitalize-out)
      (setf (case-frob-stream-sout stream)
	    #'case-frob-capitalize-sout))
    (if (lisp-stream-p target)
	(funcall (lisp-stream-sout target) target str 0 len)
	(stream-write-string target str 0 len))))

(defun case-frob-capitalize-first-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (let ((char (char-upcase char)))
	     (if (lisp-stream-p target)
		 (funcall (lisp-stream-out target) target char)
		 (stream-write-char target char)))
	   (setf (case-frob-stream-out stream)
		 #'case-frob-downcase-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-downcase-sout))
	  (t
	   (if (lisp-stream-p target)
	       (funcall (lisp-stream-out target) target char)
	       (stream-write-char target char))))))

(defun case-frob-capitalize-first-sout (stream str start end)
  (declare (type case-frob-stream stream)
	   (type simple-base-string str)
	   (type index start)
	   (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
	 (str (subseq str start end))
	 (len (length str)))
    (dotimes (i len)
      (let ((char (schar str i)))
	(when (alphanumericp char)
	  (setf (schar str i) (char-upcase char))
	  (do ((i (1+ i) (1+ i)))
	      ((= i len))
	    (setf (schar str i) (char-downcase (schar str i))))
	  (setf (case-frob-stream-out stream)
		#'case-frob-downcase-out)
	  (setf (case-frob-stream-sout stream)
		#'case-frob-downcase-sout)
	  (return))))
    (if (lisp-stream-p target)
	(funcall (lisp-stream-sout target) target str 0 len)
	(stream-write-string target str 0 len))))

;;;; stream commands

(defstruct (stream-command (:constructor make-stream-command
					 (name &optional args))
			   (:copier nil))
  (name nil :type symbol)
  (args nil :type list))
(def!method print-object ((obj stream-command) str)
  (print-unreadable-object (obj str :type t :identity t)
    (prin1 (stream-command-name obj) str)))

;;; Take a stream and wait for text or a command to appear on it. If
;;; text appears before a command, return NIL, otherwise return a
;;; command.
;;;
;;; We can't simply call the stream's misc method because NIL is an
;;; ambiguous return value: does it mean text arrived, or does it mean
;;; the stream's misc method had no :GET-COMMAND implementation? We
;;; can't return NIL until there is text input. We don't need to loop
;;; because any stream implementing :GET-COMMAND would wait until it
;;; had some input. If the LISTEN fails, then we have some stream we
;;; must wait on.
(defun get-stream-command (stream)
  (let ((cmdp (funcall (lisp-stream-misc stream) stream :get-command)))
    (cond (cmdp)
	  ((listen stream)
	   nil)
	  (t
	   ;; This waits for input and returns NIL when it arrives.
	   (unread-char (read-char stream) stream)))))

(defun read-sequence (seq stream &key (start 0) (end nil))
  #!+sb-doc
  "Destructively modify SEQ by reading elements from STREAM.
  That part of SEQ bounded by START and END is destructively modified by
  copying successive elements into it from STREAM. If the end of file
  for STREAM is reached before copying all elements of the subsequence,
  then the extra elements near the end of sequence are not updated, and
  the index of the next element is returned."
  (declare (type sequence seq)
	   (type stream stream)
	   (type index start)
	   (type sequence-end end)
	   (values index))
  (let ((end (or end (length seq))))
    (declare (type index end))
    (etypecase seq
      (list
       (let ((read-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'read-char
		  #'read-byte)))
	 (do ((rem (nthcdr start seq) (rest rem))
	      (i start (1+ i)))
	     ((or (endp rem) (>= i end)) i)
	   (declare (type list rem)
		    (type index i))
	   (let ((el (funcall read-function stream nil :eof)))
	     (when (eq el :eof)
	       (return i))
	     (setf (first rem) el)))))
      (vector
       (with-array-data ((data seq) (offset-start start) (offset-end end))
	 (typecase data
	   ((or (simple-array (unsigned-byte 8) (*))
		(simple-array (signed-byte 8) (*))
		simple-string)
	    (let* ((numbytes (- end start))
		   (bytes-read (sb!sys:read-n-bytes stream
						    data
						    offset-start
						    numbytes
						    nil)))
	      (if (< bytes-read numbytes)
		  (+ start bytes-read)
		  end)))
	   (t
	    (let ((read-function
		   (if (subtypep (stream-element-type stream) 'character)
		       #'read-char
		       #'read-byte)))
	      (do ((i offset-start (1+ i)))
		  ((>= i offset-end) end)
		(declare (type index i))
		(let ((el (funcall read-function stream nil :eof)))
		  (when (eq el :eof)
		    (return (+ start (- i offset-start))))
		  (setf (aref data i) el)))))))))))

(defun write-sequence (seq stream &key (start 0) (end nil))
  #!+sb-doc
  "Write the elements of SEQ bounded by START and END to STREAM."
  (declare (type sequence seq)
	   (type stream stream)
	   (type index start)
	   (type sequence-end end)
	   (values sequence))
  (let ((end (or end (length seq))))
    (declare (type index start end))
    (etypecase seq
      (list
       (let ((write-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'write-char
		  #'write-byte)))
	 (do ((rem (nthcdr start seq) (rest rem))
	      (i start (1+ i)))
	     ((or (endp rem) (>= i end)) seq)
	   (declare (type list rem)
		    (type index i))
	   (funcall write-function (first rem) stream))))
      (string
       (write-string* seq stream start end))
      (vector
       (let ((write-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'write-char
		  #'write-byte)))
	 (do ((i start (1+ i)))
	     ((>= i end) seq)
	   (declare (type index i))
	   (funcall write-function (aref seq i) stream)))))))

;;; (These were inline throughout this file, but that's not appropriate
;;; globally.)
(declaim (maybe-inline read-char unread-char read-byte listen))
