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

;;;; standard streams

;;; The initialization of these streams is performed by
;;; STREAM-COLD-INIT-OR-RESET.
(defvar *terminal-io* () #!+sb-doc "terminal I/O stream")
(defvar *standard-input* () #!+sb-doc "default input stream")
(defvar *standard-output* () #!+sb-doc "default output stream")
(defvar *error-output* () #!+sb-doc "error output stream")
(defvar *query-io* () #!+sb-doc "query I/O stream")
(defvar *trace-output* () #!+sb-doc "trace output stream")
(defvar *debug-io* () #!+sb-doc "interactive debugging stream")

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
(defun no-op-placeholder (&rest ignore)
  (declare (ignore ignore)))

;;; stream manipulation functions

(declaim (inline ansi-stream-input-stream-p))
(defun ansi-stream-input-stream-p (stream)
  (declare (type ansi-stream stream))

  #!+high-security
  (when (synonym-stream-p stream)
    (setf stream
	  (symbol-value (synonym-stream-symbol stream))))

  (and (not (eq (ansi-stream-in stream) #'closed-flame))
       ;;; KLUDGE: It's probably not good to have EQ tests on function
       ;;; values like this. What if someone's redefined the function?
       ;;; Is there a better way? (Perhaps just VALID-FOR-INPUT and
       ;;; VALID-FOR-OUTPUT flags? -- WHN 19990902
       (or (not (eq (ansi-stream-in stream) #'ill-in))
	   (not (eq (ansi-stream-bin stream) #'ill-bin)))))

(defun input-stream-p (stream)
  (declare (type stream stream))
  (and (ansi-stream-p stream)
       (ansi-stream-input-stream-p stream)))

(declaim (inline ansi-stream-output-stream-p))
(defun ansi-stream-output-stream-p (stream)
  (declare (type ansi-stream stream))

  #!+high-security
  (when (synonym-stream-p stream)
    (setf stream (symbol-value
		  (synonym-stream-symbol stream))))

  (and (not (eq (ansi-stream-in stream) #'closed-flame))
       (or (not (eq (ansi-stream-out stream) #'ill-out))
	   (not (eq (ansi-stream-bout stream) #'ill-bout)))))

(defun output-stream-p (stream)
  (declare (type stream stream))

  (and (ansi-stream-p stream)
       (ansi-stream-output-stream-p stream)))

(declaim (inline ansi-stream-open-stream-p))
(defun ansi-stream-open-stream-p (stream)
  (declare (type ansi-stream stream))
  (not (eq (ansi-stream-in stream) #'closed-flame)))

(defun open-stream-p (stream)
  (ansi-stream-open-stream-p stream))

(declaim (inline ansi-stream-element-type))
(defun ansi-stream-element-type (stream)
  (declare (type ansi-stream stream))
  (funcall (ansi-stream-misc stream) stream :element-type))

(defun stream-element-type (stream)
  (ansi-stream-element-type stream))

(defun interactive-stream-p (stream)
  (declare (type stream stream))
  (funcall (ansi-stream-misc stream) stream :interactive-p))

(declaim (inline ansi-stream-close))
(defun ansi-stream-close (stream abort)
  (declare (type ansi-stream stream))
  (when (open-stream-p stream)
    (funcall (ansi-stream-misc stream) stream :close abort))
  t)

(defun close (stream &key abort)
  (ansi-stream-close stream abort))

(defun set-closed-flame (stream)
  (setf (ansi-stream-in stream) #'closed-flame)
  (setf (ansi-stream-bin stream) #'closed-flame)
  (setf (ansi-stream-n-bin stream) #'closed-flame)
  (setf (ansi-stream-in stream) #'closed-flame)
  (setf (ansi-stream-out stream) #'closed-flame)
  (setf (ansi-stream-bout stream) #'closed-flame)
  (setf (ansi-stream-sout stream) #'closed-flame)
  (setf (ansi-stream-misc stream) #'closed-flame))

;;;; file position and file length

;;; Call the MISC method with the :FILE-POSITION operation.
(defun file-position (stream &optional position)
  (declare (type stream stream))
  (declare (type (or index (member nil :start :end)) position))
  (cond
   (position
    (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
    (funcall (ansi-stream-misc stream) stream :file-position position))
   (t
    (let ((res (funcall (ansi-stream-misc stream) stream :file-position nil)))
      (when res
	(- res
	   (- +ansi-stream-in-buffer-length+
	      (ansi-stream-in-index stream))))))))

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
  (funcall (ansi-stream-misc stream) stream :file-length))

;;;; input functions

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value
			    recursive-p)
  (declare (ignore recursive-p))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
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
    (if (ansi-stream-p stream)
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
    (if (ansi-stream-p stream)
	(let ((index (1- (ansi-stream-in-index stream)))
	      (buffer (ansi-stream-in-buffer stream)))
	  (declare (fixnum index))
	  (when (minusp index) (error "nothing to unread"))
	  (cond (buffer
		 (setf (aref buffer index) (char-code character))
		 (setf (ansi-stream-in-index stream) index))
		(t
		 (funcall (ansi-stream-misc stream) stream
			  :unread character))))
	;; must be Gray streams FUNDAMENTAL-STREAM
	(stream-unread-char stream character)))
  nil)


;;; In the interest of ``once and only once'' this macro contains the
;;; framework necessary to implement a peek-char function, which has
;;; two special-cases (one for gray streams and one for echo streams)
;;; in addition to the normal case.
;;;
;;; All arguments are forms which will be used for a specific purpose
;;; PEEK-TYPE - the current peek-type as defined by ANSI CL
;;; EOF-VALUE - the eof-value argument to peek-char
;;; CHAR-VAR - the variable which will be used to store the current character
;;; READ-FORM - the form which will be used to read a character
;;; UNREAD-FORM - ditto for unread-char
;;; SKIPPED-CHAR-FORM - the form to execute when skipping a character
;;; EOF-DETECTED-FORM - the form to execute when EOF has been detected
;;;                     (this will default to CHAR-VAR)
(defmacro generalized-peeking-mechanism (peek-type eof-value char-var read-form unread-form &optional (skipped-char-form nil) (eof-detected-form nil))
  `(let ((,char-var ,read-form))
    (cond ((eql ,char-var ,eof-value) 
	   ,(if eof-detected-form
		eof-detected-form
		char-var))
	  ((characterp ,peek-type)
	   (do ((,char-var ,char-var ,read-form))
	       ((or (eql ,char-var ,eof-value) 
		    (char= ,char-var ,peek-type))
		(cond ((eql ,char-var ,eof-value)
		       ,(if eof-detected-form
			    eof-detected-form
			    char-var))
		      (t ,unread-form
			 ,char-var)))
	     ,skipped-char-form))
	  ((eql ,peek-type t)
	   (do ((,char-var ,char-var ,read-form))
	       ((or (eql ,char-var ,eof-value)
		    (not (whitespace-char-p ,char-var)))
		(cond ((eql ,char-var ,eof-value)
		       ,(if eof-detected-form
			    eof-detected-form
			    char-var))
		      (t ,unread-form
			 ,char-var)))
	     ,skipped-char-form))
	  ((null ,peek-type)
	   ,unread-form
	   ,char-var)
	  (t
	   (bug "Impossible case reached in PEEK-CHAR")))))

(defun peek-char (&optional (peek-type nil)
			    (stream *standard-input*)
			    (eof-error-p t)
			    eof-value
			    recursive-p)
  (declare (ignore recursive-p))
  ;; FIXME: The type of PEEK-TYPE is also declared in a DEFKNOWN, but
  ;; the compiler doesn't seem to be smart enough to go from there to
  ;; imposing a type check. Figure out why (because PEEK-TYPE is an
  ;; &OPTIONAL argument?) and fix it, and then this explicit type
  ;; check can go away.
  (unless (typep peek-type '(or character boolean))
    (error 'simple-type-error
	   :datum peek-type
	   :expected-type '(or character boolean)
	   :format-control "~@<bad PEEK-TYPE=~S, ~_expected ~S~:>"
	   :format-arguments (list peek-type '(or character boolean))))
  (let ((stream (in-synonym-of stream)))
    (cond ((typep stream 'echo-stream)
	   (echo-misc stream 
		      :peek-char
		      peek-type
		      (list eof-error-p eof-value)))
	  ((ansi-stream-p stream)
	   (generalized-peeking-mechanism
	    peek-type eof-value char
	    (read-char stream eof-error-p eof-value)
	    (unread-char char stream)))
	  (t
	   ;; by elimination, must be Gray streams FUNDAMENTAL-STREAM
	   (generalized-peeking-mechanism
	    peek-type :eof char
	    (if (null peek-type)
		(stream-peek-char stream)
		(stream-read-char stream))
	    (if (null peek-type)
		()
		(stream-unread-char stream char))
	    ()
	    (eof-or-lose stream eof-error-p eof-value))))))

(defun listen (&optional (stream *standard-input*))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
	(or (/= (the fixnum (ansi-stream-in-index stream))
		+ansi-stream-in-buffer-length+)
	    ;; Test for T explicitly since misc methods return :EOF sometimes.
	    (eq (funcall (ansi-stream-misc stream) stream :listen) t))
	;; Fall through to Gray streams FUNDAMENTAL-STREAM case.
	(stream-listen stream))))

(defun read-char-no-hang (&optional (stream *standard-input*)
				    (eof-error-p t)
				    eof-value
				    recursive-p)
  (declare (ignore recursive-p))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
	(if (funcall (ansi-stream-misc stream) stream :listen)
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
    (cond ((ansi-stream-p stream)
	   (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
	   (funcall (ansi-stream-misc stream) stream :clear-input))
	  (t
	   (stream-clear-input stream))))
  nil)

(declaim (maybe-inline read-byte))
(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
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
  (declare (type ansi-stream stream)
	   (type index numbytes start)
	   (type (or (simple-array * (*)) system-area-pointer) buffer))
  (let* ((stream (in-synonym-of stream ansi-stream))
	 (in-buffer (ansi-stream-in-buffer stream))
	 (index (ansi-stream-in-index stream))
	 (num-buffered (- +ansi-stream-in-buffer-length+ index)))
    (declare (fixnum index num-buffered))
    (cond
     ((not in-buffer)
      (funcall (ansi-stream-n-bin stream)
	       stream
	       buffer
	       start
	       numbytes
	       eof-error-p))
     ((<= numbytes num-buffered)
      (%byte-blt in-buffer index
		 buffer start (+ start numbytes))
      (setf (ansi-stream-in-index stream) (+ index numbytes))
      numbytes)
     (t
      (let ((end (+ start num-buffered)))
	(%byte-blt in-buffer index buffer start end)
	(setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
	(+ (funcall (ansi-stream-n-bin stream)
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
(defconstant +ansi-stream-in-buffer-extra+
  4) ; FIXME: should be symbolic constant

;;; This function is called by the FAST-READ-CHAR expansion to refill
;;; the IN-BUFFER for text streams. There is definitely an IN-BUFFER,
;;; and hence must be an N-BIN method.
(defun fast-read-char-refill (stream eof-error-p eof-value)
  (let* ((ibuf (ansi-stream-in-buffer stream))
	 (count (funcall (ansi-stream-n-bin stream)
			 stream
			 ibuf
			 +ansi-stream-in-buffer-extra+
			 (- +ansi-stream-in-buffer-length+
			    +ansi-stream-in-buffer-extra+)
			 nil))
	 (start (- +ansi-stream-in-buffer-length+ count)))
    (declare (type index start count))
    (cond ((zerop count)
	   (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
	   (funcall (ansi-stream-in stream) stream eof-error-p eof-value))
	  (t
	   (when (/= start +ansi-stream-in-buffer-extra+)
	     (bit-bash-copy ibuf (+ (* +ansi-stream-in-buffer-extra+
				       sb!vm:n-byte-bits)
				    (* sb!vm:vector-data-offset
				       sb!vm:n-word-bits))
			    ibuf (+ (the index (* start sb!vm:n-byte-bits))
				    (* sb!vm:vector-data-offset
				       sb!vm:n-word-bits))
			    (* count sb!vm:n-byte-bits)))
	   (setf (ansi-stream-in-index stream) (1+ start))
	   (code-char (aref ibuf start))))))

;;; This is similar to FAST-READ-CHAR-REFILL, but we don't have to
;;; leave room for unreading.
(defun fast-read-byte-refill (stream eof-error-p eof-value)
  (let* ((ibuf (ansi-stream-in-buffer stream))
	 (count (funcall (ansi-stream-n-bin stream) stream
			 ibuf 0 +ansi-stream-in-buffer-length+
			 nil))
	 (start (- +ansi-stream-in-buffer-length+ count)))
    (declare (type index start count))
    (cond ((zerop count)
	   (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
	   (funcall (ansi-stream-bin stream) stream eof-error-p eof-value))
	  (t
	   (unless (zerop start)
	     (bit-bash-copy ibuf (* sb!vm:vector-data-offset sb!vm:n-word-bits)
			    ibuf (+ (the index (* start sb!vm:n-byte-bits))
				    (* sb!vm:vector-data-offset
				       sb!vm:n-word-bits))
			    (* count sb!vm:n-byte-bits)))
	   (setf (ansi-stream-in-index stream) (1+ start))
	   (aref ibuf start)))))

;;; output functions

(defun write-char (character &optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-out character)
		   (stream-write-char character))
  character)

(defun terpri (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-out #\newline) (stream-terpri))
  nil)

(defun fresh-line (&optional (stream *standard-output*))
  (let ((stream (out-synonym-of stream)))
    (if (ansi-stream-p stream)
	(when (/= (or (charpos stream) 1) 0)
	  (funcall (ansi-stream-out stream) stream #\newline)
	  t)
	;; must be Gray streams FUNDAMENTAL-STREAM
	(stream-fresh-line stream))))

(defun write-string (string &optional (stream *standard-output*)
			    &key (start 0) end)
  (declare (type string string))
  ;; Note that even though you might expect, based on the behavior of
  ;; things like AREF, that the correct upper bound here is
  ;; (ARRAY-DIMENSION STRING 0), the ANSI glossary definitions for
  ;; "bounding index" and "length" indicate that in this case (i.e.
  ;; for the ANSI-specified functions WRITE-STRING [and WRITE-LINE]),
  ;; (LENGTH STRING) is the required upper bound. A foolish
  ;; consistency is the hobgoblin of lesser languages..
  (%write-string string stream start (%check-vector-sequence-bounds
				      string start end))
  string)

(defun %write-string (string stream start end)
  (declare (type string string))
  (declare (type streamlike stream))
  (declare (type index start end))
  (let ((stream (out-synonym-of stream)))
    (cond ((ansi-stream-p stream)
	   (if (array-header-p string)
	       (with-array-data ((data string) (offset-start start)
				 (offset-end end))
		 (funcall (ansi-stream-sout stream)
			  stream data offset-start offset-end))
	       (funcall (ansi-stream-sout stream) stream string start end))
	   string)
	  (t ; must be Gray streams FUNDAMENTAL-STREAM
	   (stream-write-string stream string start end)))))

(defun write-line (string &optional (stream *standard-output*)
			  &key (start 0) end)
  (declare (type string string))
  ;; FIXME: Why is there this difference between the treatments of the
  ;; STREAM argument in WRITE-STRING and WRITE-LINE?
  (let ((defaulted-stream (out-synonym-of stream)))
    (%write-string string defaulted-stream start (%check-vector-sequence-bounds
						  string start end))
    (write-char #\newline defaulted-stream))
  string)

(defun charpos (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :charpos) (stream-line-column)))

(defun line-length (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :line-length)
		   (stream-line-length)))

(defun finish-output (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :finish-output)
		   (stream-finish-output))
  nil)

(defun force-output (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :force-output)
		   (stream-force-output))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :clear-output)
		   (stream-force-output))
  nil)

(defun write-byte (integer stream)
  (with-out-stream stream (ansi-stream-bout integer)
		   (stream-write-byte integer))
  integer)

;;; This is called from ANSI-STREAM routines that encapsulate CLOS
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

(defstruct (broadcast-stream (:include ansi-stream
				       (out #'broadcast-out)
				       (bout #'broadcast-bout)
				       (sout #'broadcast-sout)
				       (misc #'broadcast-misc))
			     (:constructor %make-broadcast-stream
					   (&rest streams))
			     (:copier nil))
  ;; a list of all the streams we broadcast to
  (streams () :type list :read-only t))

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
		  (if (ansi-stream-p stream)
		      (funcall (,method stream) stream ,@args)
		      (,stream-method stream ,@args))))))
  (out-fun broadcast-out ansi-stream-out stream-write-char char)
  (out-fun broadcast-bout ansi-stream-bout stream-write-byte byte)
  (out-fun broadcast-sout ansi-stream-sout stream-write-string
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
		 (if (ansi-stream-p stream)
		     (funcall (ansi-stream-misc stream) stream operation
			      arg1 arg2)
		     (stream-misc-dispatch stream operation arg1 arg2)))))))))

;;;; synonym streams

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
(def!method print-object ((x synonym-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":SYMBOL ~S" (synonym-stream-symbol x))))

;;; The output simple output methods just call the corresponding method
;;; in the synonymed stream.
(macrolet ((out-fun (name slot stream-method &rest args)
	     `(defun ,name (stream ,@args)
		(declare (optimize (safety 1)))
		(let ((syn (symbol-value (synonym-stream-symbol stream))))
		  (if (ansi-stream-p syn)
		      (funcall (,slot syn) syn ,@args)
		      (,stream-method syn ,@args))))))
  (out-fun synonym-out ansi-stream-out stream-write-char ch)
  (out-fun synonym-bout ansi-stream-bout stream-write-byte n)
  (out-fun synonym-sout ansi-stream-sout stream-write-string string start end))

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
    (if (ansi-stream-p syn)
	;; We have to special-case some operations which interact with
	;; the in-buffer of the wrapped stream, since just calling
	;; ANSI-STREAM-MISC on them
 	(case operation
	  (:listen (or (/= (the fixnum (ansi-stream-in-index syn))
			   +ansi-stream-in-buffer-length+)
		       (funcall (ansi-stream-misc syn) syn :listen)))
          (:clear-input (clear-input syn))
          (:unread (unread-char arg1 syn))
	  (t
	   (funcall (ansi-stream-misc syn) syn operation arg1 arg2)))
	(stream-misc-dispatch syn operation arg1 arg2))))

;;;; two-way streams

(defstruct (two-way-stream
	    (:include ansi-stream
		      (in #'two-way-in)
		      (bin #'two-way-bin)
		      (n-bin #'two-way-n-bin)
		      (out #'two-way-out)
		      (bout #'two-way-bout)
		      (sout #'two-way-sout)
		      (misc #'two-way-misc))
	    (:constructor %make-two-way-stream (input-stream output-stream))
	    (:copier nil))
  (input-stream (missing-arg) :type stream :read-only t)
  (output-stream (missing-arg) :type stream :read-only t))
(defprinter (two-way-stream) input-stream output-stream)

(defun make-two-way-stream (input-stream output-stream)
  #!+sb-doc
  "Return a bidirectional stream which gets its input from INPUT-STREAM and
   sends its output to OUTPUT-STREAM."
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
		  (if (ansi-stream-p syn)
		      (funcall (,slot syn) syn ,@args)
		      (,stream-method syn ,@args))))))
  (out-fun two-way-out ansi-stream-out stream-write-char ch)
  (out-fun two-way-bout ansi-stream-bout stream-write-byte n)
  (out-fun two-way-sout ansi-stream-sout stream-write-string string start end))

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
	 (in-ansi-stream-p (ansi-stream-p in))
	 (out-ansi-stream-p (ansi-stream-p out)))
    (case operation
      (:listen
       (if in-ansi-stream-p
	   (or (/= (the fixnum (ansi-stream-in-index in))
		   +ansi-stream-in-buffer-length+)
	       (funcall (ansi-stream-misc in) in :listen))
	   (stream-listen in)))
      ((:finish-output :force-output :clear-output)
       (if out-ansi-stream-p
	   (funcall (ansi-stream-misc out) out operation arg1 arg2)
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
       (or (if in-ansi-stream-p
	       (funcall (ansi-stream-misc in) in operation arg1 arg2)
	       (stream-misc-dispatch in operation arg1 arg2))
	   (if out-ansi-stream-p
	       (funcall (ansi-stream-misc out) out operation arg1 arg2)
	       (stream-misc-dispatch out operation arg1 arg2)))))))

;;;; concatenated streams

(defstruct (concatenated-stream
	    (:include ansi-stream
		      (in #'concatenated-in)
		      (bin #'concatenated-bin)
		      (n-bin #'concatenated-n-bin)
		      (misc #'concatenated-misc))
	    (:constructor %make-concatenated-stream
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

(defun make-concatenated-stream (&rest streams)
  #!+sb-doc
  "Return a stream which takes its input from each of the streams in turn,
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
	     (let ((stuff (if (ansi-stream-p current)
			      (funcall (ansi-stream-misc current) current
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
	   (if (ansi-stream-p current)
	       (funcall (ansi-stream-misc current) current operation arg1 arg2)
	       (stream-misc-dispatch current operation arg1 arg2))))))))

;;;; echo streams

(defstruct (echo-stream
	    (:include two-way-stream
		      (in #'echo-in)
		      (bin #'echo-bin)
		      (misc #'echo-misc)
		      (n-bin #'ill-bin))
	    (:constructor %make-echo-stream (input-stream output-stream))
	    (:copier nil))
  unread-stuff)
(def!method print-object ((x echo-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream
	    ":INPUT-STREAM ~S :OUTPUT-STREAM ~S"
	    (two-way-stream-input-stream x)
	    (two-way-stream-output-stream x))))

(defun make-echo-stream (input-stream output-stream)
  #!+sb-doc
  "Return a bidirectional stream which gets its input from INPUT-STREAM and
   sends its output to OUTPUT-STREAM. In addition, all input is echoed to
   the output stream."
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
  (funcall #'%make-echo-stream input-stream output-stream))

(macrolet ((in-fun (name fun out-slot stream-method &rest args)
	     `(defun ,name (stream ,@args)
		(or (pop (echo-stream-unread-stuff stream))
		    (let* ((in (echo-stream-input-stream stream))
			   (out (echo-stream-output-stream stream))
			   (result (,fun in ,@args)))
		      (if (ansi-stream-p out)
			  (funcall (,out-slot out) out result)
			  (,stream-method out result))
		      result)))))
  (in-fun echo-in read-char ansi-stream-out stream-write-char
	  eof-error-p eof-value)
  (in-fun echo-bin read-byte ansi-stream-bout stream-write-byte
	  eof-error-p eof-value))

(defun echo-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
	 (out (two-way-stream-output-stream stream)))
    (case operation
      (:listen
       (or (not (null (echo-stream-unread-stuff stream)))
	   (if (ansi-stream-p in)
	       (or (/= (the fixnum (ansi-stream-in-index in))
		       +ansi-stream-in-buffer-length+)
		   (funcall (ansi-stream-misc in) in :listen))
	       (stream-misc-dispatch in :listen))))
      (:unread (push arg1 (echo-stream-unread-stuff stream)))
      (:element-type
       (let ((in-type (stream-element-type in))
	     (out-type (stream-element-type out)))
	 (if (equal in-type out-type)
	     in-type `(and ,in-type ,out-type))))
      (:close
       (set-closed-flame stream))
      (:peek-char
       ;; For the special case of peeking into an echo-stream
       ;; arg1 is PEEK-TYPE, arg2 is (EOF-ERROR-P EOF-VALUE)
       ;; returns peeked-char, eof-value, or errors end-of-file
       ;;
       ;; Note: This code could be moved into PEEK-CHAR if desired.
       ;; I am unsure whether this belongs with echo-streams because it is
       ;; echo-stream specific, or PEEK-CHAR because it is peeking code.
       ;; -- mrd 2002-11-18
       ;;
       ;; UNREAD-CHAR-P indicates whether the current character was one
       ;; that was previously unread.  In that case, we need to ensure that
       ;; the semantics for UNREAD-CHAR are held; the character should
       ;; not be echoed again.
       (let ((unread-char-p nil))
	 (flet ((outfn (c)
		  (unless unread-char-p
		    (if (ansi-stream-p out)
			(funcall (ansi-stream-out out) out c)
			;; gray-stream
			(stream-write-char out c))))
		(infn ()
		  ;; Obtain input from unread buffer or input stream,
		  ;; and set the flag appropriately.
		  (cond ((not (null (echo-stream-unread-stuff stream)))
			 (setf unread-char-p t)
			 (pop (echo-stream-unread-stuff stream)))
			(t
			 (setf unread-char-p nil)
			 (read-char in (first arg2) (second arg2))))))
	   (generalized-peeking-mechanism
	    arg1 (second arg2) char
	    (infn)
	    (unread-char char in)
	    (outfn char)))))
      (t
       (or (if (ansi-stream-p in)
	       (funcall (ansi-stream-misc in) in operation arg1 arg2)
	       (stream-misc-dispatch in operation arg1 arg2))
	   (if (ansi-stream-p out)
	       (funcall (ansi-stream-misc out) out operation arg1 arg2)
	       (stream-misc-dispatch out operation arg1 arg2)))))))

;;;; base STRING-STREAM stuff

(defstruct (string-stream
             (:include ansi-stream)
             (:constructor nil)
             (:copier nil))
  (string nil :type string))

;;;; STRING-INPUT-STREAM stuff

(defstruct (string-input-stream
	     (:include string-stream
		       (in #'string-inch)
		       (bin #'string-binch)
		       (n-bin #'string-stream-read-n-bytes)
		       (misc #'string-in-misc)
                       (string nil :type simple-string))
	     (:constructor internal-make-string-input-stream
			   (string current end))
	     (:copier nil))
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
			 (* index sb!vm:n-byte-bits)
			 (if (typep buffer 'system-area-pointer)
			     buffer
			     (vector-sap buffer))
			 (* start sb!vm:n-byte-bits)
			 (* copy sb!vm:n-byte-bits))))
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
					(start 0) end)
  #!+sb-doc
  "Return an input stream which will supply the characters of STRING between
  START and END in order."
  (declare (type string string)
	   (type index start)
	   (type (or index null) end))
  
  (internal-make-string-input-stream
   (coerce string 'simple-string)
   start
   (%check-vector-sequence-bounds string start end)))

;;;; STRING-OUTPUT-STREAM stuff

(defstruct (string-output-stream
	    (:include string-stream
		      (out #'string-ouch)
		      (sout #'string-sout)
		      (misc #'string-out-misc)
                      ;; The string we throw stuff in.
                      (string (make-string 40) :type simple-string))
	    (:constructor make-string-output-stream ())
	    (:copier nil))
  ;; Index of the next location to use.
  (index 0 :type fixnum))

#!+sb-doc
(setf (fdocumentation 'make-string-output-stream 'function)
  "Return an output stream which will accumulate all output given it for
   the benefit of the function GET-OUTPUT-STREAM-STRING.")

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
  (%write-string (string-output-stream-string in-stream)
		 out-stream
		 0
		 (string-output-stream-index in-stream))
  (setf (string-output-stream-index in-stream) 0))

;;;; fill-pointer streams

;;; Fill pointer STRING-OUTPUT-STREAMs are not explicitly mentioned in
;;; the CLM, but they are required for the implementation of
;;; WITH-OUTPUT-TO-STRING.

(deftype string-with-fill-pointer ()
  '(and string
	(satisfies array-has-fill-pointer-p)))

(defstruct (fill-pointer-output-stream
 	    (:include string-stream
		      (out #'fill-pointer-ouch)
		      (sout #'fill-pointer-sout)
		      (misc #'fill-pointer-misc)
                      ;; a string with a fill pointer where we stuff
                      ;; the stuff we write
                      (string (error "missing argument")
                              :type string-with-fill-pointer
                              :read-only t))
	    (:constructor make-fill-pointer-output-stream (string))
	    (:copier nil)))

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
	    (let* ((new-length (1+ (* current 2)))
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

(defstruct (indenting-stream (:include ansi-stream
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
 "Return an output stream which indents its output by some amount.")

;;; INDENTING-INDENT writes the correct number of spaces needed to indent
;;; output on the given STREAM based on the specified SUB-STREAM.
(defmacro indenting-indent (stream sub-stream)
  ;; KLUDGE: bare magic number 60
  `(do ((i 0 (+ i 60))
	(indentation (indenting-stream-indentation ,stream)))
       ((>= i indentation))
     (%write-string
      "							    "
      ,sub-stream
      0
      (min 60 (- indentation i)))))

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
	     (%write-string string sub-stream i (1+ newline))
	     (indenting-indent stream sub-stream)
	     (setq i (+ newline 1)))
	    (t
	     (%write-string string sub-stream i end)
	     (setq i end))))))

;;; INDENTING-MISC just treats just the :LINE-LENGTH message
;;; differently. INDENTING-CHARPOS says the charpos is the charpos of
;;; the base stream minus the stream's indentation.
(defun indenting-misc (stream operation &optional arg1 arg2)
  (let ((sub-stream (indenting-stream-stream stream)))
    (if (ansi-stream-p sub-stream)
	(let ((method (ansi-stream-misc sub-stream)))
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
	    (:include ansi-stream
		      (misc #'case-frob-misc))
	    (:constructor %make-case-frob-stream (target out sout))
	    (:copier nil))
  (target (missing-arg) :type stream))

(defun make-case-frob-stream (target kind)
  #!+sb-doc
  "Return a stream that sends all output to the stream TARGET, but modifies
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
       (if (ansi-stream-p target)
	   (funcall (ansi-stream-misc target) target op arg1 arg2)
	   (stream-misc-dispatch target op arg1 arg2))))))

(defun case-frob-upcase-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream))
	(char (char-upcase char)))
    (if (ansi-stream-p target)
	(funcall (ansi-stream-out target) target char)
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
    (if (ansi-stream-p target)
	(funcall (ansi-stream-sout target) target string 0 string-len)
	(stream-write-string target string 0 string-len))))

(defun case-frob-downcase-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream))
	(char (char-downcase char)))
    (if (ansi-stream-p target)
	(funcall (ansi-stream-out target) target char)
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
    (if (ansi-stream-p target)
	(funcall (ansi-stream-sout target) target string 0 string-len)
	(stream-write-string target string 0 string-len))))

(defun case-frob-capitalize-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (let ((char (char-upcase char)))
	     (if (ansi-stream-p target)
		 (funcall (ansi-stream-out target) target char)
		 (stream-write-char target char)))
	   (setf (case-frob-stream-out stream) #'case-frob-capitalize-aux-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-capitalize-aux-sout))
	  (t
	   (if (ansi-stream-p target)
	       (funcall (ansi-stream-out target) target char)
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
    (if (ansi-stream-p target)
	(funcall (ansi-stream-sout target) target str 0 len)
	(stream-write-string target str 0 len))))

(defun case-frob-capitalize-aux-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (let ((char (char-downcase char)))
	     (if (ansi-stream-p target)
		 (funcall (ansi-stream-out target) target char)
		 (stream-write-char target char))))
	  (t
	   (if (ansi-stream-p target)
	       (funcall (ansi-stream-out target) target char)
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
    (if (ansi-stream-p target)
	(funcall (ansi-stream-sout target) target str 0 len)
	(stream-write-string target str 0 len))))

(defun case-frob-capitalize-first-out (stream char)
  (declare (type case-frob-stream stream)
	   (type base-char char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
	   (let ((char (char-upcase char)))
	     (if (ansi-stream-p target)
		 (funcall (ansi-stream-out target) target char)
		 (stream-write-char target char)))
	   (setf (case-frob-stream-out stream)
		 #'case-frob-downcase-out)
	   (setf (case-frob-stream-sout stream)
		 #'case-frob-downcase-sout))
	  (t
	   (if (ansi-stream-p target)
	       (funcall (ansi-stream-out target) target char)
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
    (if (ansi-stream-p target)
	(funcall (ansi-stream-sout target) target str 0 len)
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
  (let ((cmdp (funcall (ansi-stream-misc stream) stream :get-command)))
    (cond (cmdp)
	  ((listen stream)
	   nil)
	  (t
	   ;; This waits for input and returns NIL when it arrives.
	   (unread-char (read-char stream) stream)))))

;;;; READ-SEQUENCE

(defun read-sequence (seq stream &key (start 0) end)
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
  (if (ansi-stream-p stream)
      (ansi-stream-read-sequence seq stream start end)
      ;; must be Gray streams FUNDAMENTAL-STREAM
      (stream-read-sequence stream seq start end)))

(defun ansi-stream-read-sequence (seq stream start %end)
  (declare (type sequence seq)
	   (type ansi-stream stream)
	   (type index start)
	   (type sequence-end %end)
	   (values index))
  (let ((end (or %end (length seq))))
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

;;;; WRITE-SEQUENCE

(defun write-sequence (seq stream &key (start 0) (end nil))
  #!+sb-doc
  "Write the elements of SEQ bounded by START and END to STREAM."
  (declare (type sequence seq)
	   (type stream stream)
	   (type index start)
	   (type sequence-end end)
	   (values sequence))
  (if (ansi-stream-p stream)
      (ansi-stream-write-sequence seq stream start end)
      ;; must be Gray-streams FUNDAMENTAL-STREAM
      (stream-write-sequence stream seq start end)))

(defun ansi-stream-write-sequence (seq stream start %end)
  (declare (type sequence seq)
	   (type ansi-stream stream)
	   (type index start)
	   (type sequence-end %end)
	   (values sequence))
  (let ((end (or %end (length seq))))
    (declare (type index end))
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
       (%write-string seq stream start end))
      (vector
       (let ((write-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'write-char
		  #'write-byte)))
	 (do ((i start (1+ i)))
	     ((>= i end) seq)
	   (declare (type index i))
	   (funcall write-function (aref seq i) stream)))))))

;;;; etc.

;;; (These were inline throughout this file, but that's not appropriate
;;; globally.)
(declaim (maybe-inline read-char unread-char read-byte listen))
