;;; -*- lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

(in-package "SB-SIMPLE-STREAMS")


;;; Basic functionality for ansi-streams.  These are separate
;;; functions because they are called in places where we already know
;;; we operate on an ansi-stream (as opposed to a simple- or
;;; gray-stream, or the symbols t or nil), so we can evade typecase
;;; and (in|out)-synonym-of calls.

(declaim (inline %ansi-stream-read-byte %ansi-stream-read-char
                 %ansi-stream-unread-char %ansi-stream-read-line
                 %ansi-stream-read-sequence))

(defun %ansi-stream-read-byte (stream eof-error-p eof-value blocking)
  (declare (ignore blocking))
  #+nil
  (sb-kernel:ansi-stream-read-byte stream eof-error-p eof-value)
  (sb-int:prepare-for-fast-read-byte stream
    (prog1
        (sb-int:fast-read-byte eof-error-p eof-value t)
      (sb-int:done-with-fast-read-byte))))

(defun %ansi-stream-read-char (stream eof-error-p eof-value blocking)
  (declare (ignore blocking))
  #+nil
  (sb-kernel:ansi-stream-read-char stream eof-error-p eof-value)
  (sb-int:prepare-for-fast-read-char stream
    (prog1
        (sb-int:fast-read-char eof-error-p eof-value)
      (sb-int:done-with-fast-read-char))))

(defun %ansi-stream-unread-char (character stream)
  (let ((index (1- (sb-kernel:ansi-stream-in-index stream)))
        (buffer (sb-kernel:ansi-stream-in-buffer stream)))
    (declare (fixnum index))
    (when (minusp index) (error "nothing to unread"))
    (cond (buffer
           (setf (aref buffer index) (char-code character))
           (setf (sb-kernel:ansi-stream-in-index stream) index))
          (t
           (funcall (sb-kernel:ansi-stream-misc stream) stream
                    :unread character)))))

(defun %ansi-stream-read-line (stream eof-error-p eof-value)
  (sb-int:prepare-for-fast-read-char stream
    (let ((res (make-string 80))
          (len 80)
          (index 0))
      (loop
       (let ((ch (sb-int:fast-read-char nil nil)))
         (cond (ch
                (when (char= ch #\newline)
                  (sb-int:done-with-fast-read-char)
                  (return (values (sb-kernel:shrink-vector res index) nil)))
                (when (= index len)
                  (setq len (* len 2))
                  (let ((new (make-string len)))
                    (replace new res)
                    (setq res new)))
                (setf (schar res index) ch)
                (incf index))
               ((zerop index)
                (sb-int:done-with-fast-read-char)
                (return (values (sb-impl::eof-or-lose stream eof-error-p
                                                      eof-value)
                                t)))
               ;; Since FAST-READ-CHAR already hit the eof char, we
               ;; shouldn't do another READ-CHAR.
               (t
                (sb-int:done-with-fast-read-char)
                (return (values (sb-kernel:shrink-vector res index) t)))))))))

(defun %ansi-stream-read-sequence (seq stream start %end)
  (declare (type sequence seq)
	   (type sb-kernel:ansi-stream stream)
	   (type sb-int:index start)
	   (type sb-kernel:sequence-end %end)
	   (values sb-int:index))
  (let ((end (or %end (length seq))))
    (declare (type sb-int:index end))
    (etypecase seq
      (list
       (let ((read-function
	      (if (subtypep (stream-element-type stream) 'character)
		  #'%ansi-stream-read-char
		  #'%ansi-stream-read-byte)))
	 (do ((rem (nthcdr start seq) (rest rem))
	      (i start (1+ i)))
	     ((or (endp rem) (>= i end)) i)
	   (declare (type list rem)
		    (type sb-int:index i))
	   (let ((el (funcall read-function stream nil :eof)))
	     (when (eq el :eof)
	       (return i))
	     (setf (first rem) el)))))
      (vector
       (sb-kernel:with-array-data ((data seq) (offset-start start)
                                   (offset-end end))
         (typecase data
	   ((or (simple-array (unsigned-byte 8) (*))
		(simple-array (signed-byte 8) (*))
		simple-string)
	    (let* ((numbytes (- end start))
		   (bytes-read (sb-sys:read-n-bytes stream
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
		       #'%ansi-stream-read-char
		       #'%ansi-stream-read-byte)))
	      (do ((i offset-start (1+ i)))
		  ((>= i offset-end) end)
		(declare (type sb-int:index i))
		(let ((el (funcall read-function stream nil :eof)))
		  (when (eq el :eof)
		    (return (+ start (- i offset-start))))
		  (setf (aref data i) el)))))))))))


(defun %ansi-stream-write-string (string stream start end)
  (declare (type string string)
           (type sb-kernel:ansi-stream stream)
           (type sb-int:index start end))

  ;; Note that even though you might expect, based on the behavior of
  ;; things like AREF, that the correct upper bound here is
  ;; (ARRAY-DIMENSION STRING 0), the ANSI glossary definitions for
  ;; "bounding index" and "length" indicate that in this case (i.e.
  ;; for the ANSI-specified functions WRITE-STRING and WRITE-LINE
  ;; which are implemented in terms of this function), (LENGTH STRING)
  ;; is the required upper bound. A foolish consistency is the
  ;; hobgoblin of lesser languages..
  (unless (<= 0 start end (length string))
    (error "~@<bad bounding indices START=~W END=~W for ~2I~_~S~:>"
	   start
	   end
	   string))

  (if (sb-kernel:array-header-p string)
      (sb-kernel:with-array-data ((data string) (offset-start start)
                                  (offset-end end))
        (funcall (sb-kernel:ansi-stream-sout stream)
                 stream data offset-start offset-end))
      (funcall (sb-kernel:ansi-stream-sout stream) stream string start end))
  string)

(defun %ansi-stream-write-sequence (seq stream start %end)
  (declare (type sequence seq)
           (type sb-kernel:ansi-stream stream)
           (type sb-int:index start)
           (type sb-kernel:sequence-end %end)
           (values sequence))
  (let ((end (or %end (length seq))))
    (declare (type sb-int:index end))
    (etypecase seq
      (list
       (let ((write-function
	      (if (subtypep (stream-element-type stream) 'character)
                  ;; TODO: Replace these with ansi-stream specific
                  ;; functions too.
		  #'write-char
		  #'write-byte)))
	 (do ((rem (nthcdr start seq) (rest rem))
	      (i start (1+ i)))
	     ((or (endp rem) (>= i end)) seq)
	   (declare (type list rem)
		    (type sb-int:index i))
	   (funcall write-function (first rem) stream))))
      (string
       (%ansi-stream-write-string seq stream start end))
      (vector
       (let ((write-function
	      (if (subtypep (stream-element-type stream) 'character)
                  ;; TODO: Replace these with ansi-stream specific
                  ;; functions too.
		  #'write-char
		  #'write-byte)))
	 (do ((i start (1+ i)))
	     ((>= i end) seq)
	   (declare (type sb-int:index i))
	   (funcall write-function (aref seq i) stream)))))))


;;;
;;; USER-LEVEL FUNCTIONS
;;;

(defmethod open-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :input :output))

(defmethod input-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :input))

(defmethod output-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :output))

(defmethod stream-element-type ((stream simple-stream))
  '(unsigned-byte 8))

(defun interactive-stream-p (stream)
  "Return true if Stream does I/O on a terminal or other interactive device."
  (declare (type stream stream))
  (etypecase stream
    (simple-stream
     (any-stream-instance-flags stream :interactive))
    (ansi-stream
     (funcall (sb-kernel:ansi-stream-misc stream) stream :interactive-p))
    (fundamental-stream nil)))

(defun (setf interactive-stream-p) (value stream)
  (etypecase stream
    (simple-stream
     (if value
	 (add-stream-instance-flags stream :interactive)
	 (remove-stream-instance-flags stream :interactive)))))

(defun stream-external-format (stream)
  "Returns Stream's external-format."
  (declare (type stream stream))
  (etypecase stream
    (simple-stream
     (with-stream-class (simple-stream)
       (sm external-format stream)))
    (ansi-stream
     :default)
    (fundamental-stream #| not defined on Gray streams? |#
     :default)))

(defgeneric default-open-class (name &optional element-type)
  (:documentation
   "Determine the stream class to be created when an attempt is made
to open NAME.  This is a CMUCL- and SBCL-specific extension to Franz's
simple-streams proposal.")
  (:method ((name t) &optional element-type)
     (declare (ignore element-type))
     nil)
  (:method ((name pathname) &optional element-type)
     (declare (ignore element-type))
     'sb-sys::file-stream)
  (:method ((name string) &optional element-type)
     (declare (ignore element-type))
     'sb-sys::file-stream)
  (:method ((name stream) &optional element-type)
     (declare (ignore element-type))
     (class-name (class-of name))))

(defun open (filename &rest options
             &key (direction :input)
             (element-type 'character element-type-given)
             if-exists if-does-not-exist
             (external-format :default)
             class mapped input-handle output-handle
             &allow-other-keys)
  "Return a stream which reads from or writes to Filename.
  Defined keywords:
   :direction - one of :input, :output, :io, or :probe
   :element-type - type of object to read or write, default BASE-CHAR
   :if-exists - one of :error, :new-version, :rename, :rename-and-delete,
                       :overwrite, :append, :supersede or NIL
   :if-does-not-exist - one of :error, :create or NIL
   :external-format - :default
  See the manual for details.

  The following are simple-streams-specific additions:
   :class - class of stream object to be created
   :mapped - T to open a memory-mapped file
   :input-handle - a stream or Unix file descriptor to read from
   :output-handle - a stream or Unix file descriptor to write to

  If Class is NIL or not supplied, DEFAULT-OPEN-CLASS is called on
  Filename to determine its value, thus Filename need not be an actual
  file name; it could be any arbitrary user-defined object for which a
  method of DEFAULT-OPEN-CLASS is applicable."
  (declare (ignore if-exists if-does-not-exist external-format
		   input-handle output-handle))
  (let ((klass class)
	(options (copy-list options))
	(filespec (if (stringp filename) (parse-filespec filename) filename)))
    (unless klass
      (setq klass (default-open-class filespec (if element-type-given
						   element-type
						   nil))))
    (unless klass
      (error 'type-error :datum filename
	     :expected-type '(or pathname stream base-string)))
    (cond ((eql klass 'sb-sys::file-stream)
	   (remf options :class)
	   (remf options :mapped)
	   ;; INPUT-HANDLE and OUTPUT-HANDLE must be fixnums or NIL.
	   ;; If both are given, they must be the same -- or maybe
	   ;; we should make a TWO-WAY-STREAM in that case??
	   ;; If they are given, use SYS:MAKE-FD-STREAM to make the
	   ;; stream.  Direction must be appropriate, too.
	   (remf options :input-handle)
	   (remf options :output-handle)
	   (apply #'open-fd-stream filespec options))
	  ((subtypep klass 'simple-stream)
	   (when element-type-given
	     (error "Can't create simple-streams with an element-type."))
	   (when (and (eq klass 'file-simple-stream) mapped)
	     (setq klass 'mapped-file-simple-stream)
	     (setf (getf options :class) 'mapped-file-simple-stream))
	   (when (subtypep klass 'file-simple-stream)
	     (when (eq direction :probe)
	       (setq klass 'probe-simple-stream)))
	   (apply #'make-instance klass (list* :filename filespec options)))
	  ((subtypep klass 'fundamental-stream)
	   (error "Gray streams are not supported by OPEN."))
	  (t
	   (if class
	       (error "Unable to open streams of class ~S." class)
	       (error "DEFAULT-OPEN-CLASS method on ~S instances is broken!"
		      (class-name (class-of filespec))))))))

(defmacro %check-simple-stream (stream &optional direction)
  ;; Check that STREAM is valid and open in the appropriate direction.
  `(locally
     (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
     (with-stream-class (simple-stream ,stream)
       (let ((flags (sm %flags ,stream)))
	 (cond ((zerop (logand flags ,(%flags '(:simple))))
		(error "~S is not properly initialized." stream))
	       ((zerop (logand flags ,(%flags '(:input :output))))
		(error "~S is closed." stream))
	       ,@(when direction
		   `(((zerop (logand flags ,(%flags (list direction))))
		      (error ,(format nil "~~S is not an ~(~A~) stream."
				      direction)
			     stream)))))))))

(declaim (inline sc-read-byte dc-read-byte))
(defun sc-read-byte (stream eof-error-p eof-value blocking)
  (with-stream-class (single-channel-simple-stream stream)
    ;; @@1
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
	(let ((bytes (device-read stream nil 0 nil blocking)))
	  (declare (type fixnum bytes))
	  (if (plusp bytes)
	      (setf (sm buffer-ptr stream) bytes
		    ptr 0)
	      (return-from sc-read-byte
		(sb-impl::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (bref (sm buffer stream) ptr))))

(defun dc-read-byte (stream eof-error-p eof-value blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
	(let ((bytes (device-read stream nil 0 nil blocking)))
	  (declare (type fixnum bytes))
	  (if (plusp bytes)
	      (setf (sm buffer-ptr stream) bytes
		    ptr 0)
	      (return-from dc-read-byte
		(sb-impl::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (bref (sm buffer stream) ptr))))

(declaim (inline read-byte read-char read-char-no-hang unread-char))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  "Returns the next byte of the Stream."
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :input)
       (with-stream-class (simple-stream stream)
	 (cond ((any-stream-instance-flags stream :eof)
		(sb-impl::eof-or-lose stream eof-error-p eof-value))
	       ((any-stream-instance-flags stream :string)
		(with-stream-class (string-simple-stream stream)
		  (let ((encap (sm input-handle stream)))
		    (unless encap
		      (error "Can't read-byte on string streams"))
		    (prog1
			(locally (declare (notinline read-byte))
			  (read-byte encap eof-error-p eof-value))
		      (setf (sm last-char-read-size stream) 0
			    (sm encapsulated-char-read-size stream) 0)))))
	       ((any-stream-instance-flags stream :dual)
		(dc-read-byte stream eof-error-p eof-value t))
	       (t ;; single-channel-simple-stream
		(sc-read-byte stream eof-error-p eof-value t)))))
      (ansi-stream
       (%ansi-stream-read-byte stream eof-error-p eof-value t))
      (fundamental-stream
       (let ((char (sb-gray:stream-read-byte stream)))
	 (if (eq char :eof)
	     (sb-impl::eof-or-lose stream eof-error-p eof-value)
	     char))))))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t)
			    eof-value recursive-p)
  "Inputs a character from Stream and returns it."
  (declare (ignore recursive-p))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :input)
       (with-stream-class (simple-stream)
	 (funcall-stm-handler j-read-char stream eof-error-p eof-value t)))
      (ansi-stream
       (%ansi-stream-read-char stream eof-error-p eof-value t))
      (fundamental-stream
       (let ((char (sb-gray:stream-read-char stream)))
	 (if (eq char :eof)
	     (sb-impl::eof-or-lose stream eof-error-p eof-value)
	     char))))))

(defun read-char-no-hang (&optional (stream *standard-input*) (eof-error-p t)
				    eof-value recursive-p)
  "Returns the next character from the Stream if one is availible, or nil."
  (declare (ignore recursive-p))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :input)
       (with-stream-class (simple-stream)
	 (funcall-stm-handler j-read-char stream eof-error-p eof-value nil)))
      (ansi-stream
       (if (funcall (sb-kernel:ansi-stream-misc stream) stream :listen)
           (%ansi-stream-read-char stream eof-error-p eof-value t)
           nil))
      (fundamental-stream
       (let ((char (sb-gray:stream-read-char-no-hang stream)))
	 (if (eq char :eof)
	     (sb-impl::eof-or-lose stream eof-error-p eof-value)
	     char))))))

(defun unread-char (character &optional (stream *standard-input*))
  "Puts the Character back on the front of the input Stream."
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :input)
       (with-stream-class (simple-stream)
	 (if (zerop (sm last-char-read-size stream))
	     (error "Nothing to unread.")
	     (funcall-stm-handler j-unread-char stream nil))))
      (ansi-stream
       (%ansi-stream-unread-char character stream))
      (fundamental-stream
       (sb-gray:stream-unread-char stream character))))
  nil)

(declaim (notinline read-byte read-char read-char-no-hang unread-char))

(defun peek-char (&optional (peek-type nil) (stream *standard-input*)
			    (eof-error-p t) eof-value recursive-p)
  "Peeks at the next character in the input Stream.  See manual for details."
  (declare (ignore recursive-p))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :input)
       (with-stream-class (simple-stream)
	 (let ((char (funcall-stm-handler j-read-char stream
					  eof-error-p eof-value t)))
	   (cond ((eq char eof-value) char)
		 ((characterp peek-type)
		  (do ((char char (funcall-stm-handler j-read-char stream
						       eof-error-p
						       eof-value t)))
		      ((or (eq char eof-value) (char= char peek-type))
		       (unless (eq char eof-value)
			 (funcall-stm-handler j-unread-char stream t))
		       char)))
		 ((eq peek-type t)
		  (do ((char char (funcall-stm-handler j-read-char stream
						       eof-error-p
						       eof-value t)))
		      ((or (eq char eof-value)
			   (not (sb-int:whitespace-char-p char)))
		       (unless (eq char eof-value)
			 (funcall-stm-handler j-unread-char stream t))
		       char)))
		 (t
		  (funcall-stm-handler j-unread-char stream t)
		  char)))))
      (ansi-stream
       (let ((char (%ansi-stream-read-char stream eof-error-p eof-value t)))
          (cond ((eq char eof-value) char)
                ((characterp peek-type)
                 (do ((char char (%ansi-stream-read-char stream eof-error-p
                                                         eof-value t)))
                     ((or (eq char eof-value) (char= char peek-type))
                      (unless (eq char eof-value)
                        (%ansi-stream-unread-char char stream))
                      char)))
                ((eq peek-type t)
                 (do ((char char (%ansi-stream-read-char stream eof-error-p
                                                         eof-value t)))
                     ((or (eq char eof-value)
			  (not (sb-int:whitespace-char-p char)))
                      (unless (eq char eof-value)
                        (%ansi-stream-unread-char char stream))
                      char)))
                (t
                 (%ansi-stream-unread-char char stream)
                 char))))
      (fundamental-stream
       (cond ((characterp peek-type)
	      (do ((char (sb-gray:stream-read-char stream)
			 (sb-gray:stream-read-char stream)))
		  ((or (eq char :eof) (char= char peek-type))
		   (cond ((eq char :eof)
			  (sb-impl::eof-or-lose stream eof-error-p eof-value))
			 (t
			  (sb-gray:stream-unread-char stream char)
			  char)))))
	     ((eq peek-type t)
	      (do ((char (sb-gray:stream-read-char stream)
			 (sb-gray:stream-read-char stream)))
		  ((or (eq char :eof) (not (sb-int:whitespace-char-p char)))
		   (cond ((eq char :eof)
			  (sb-impl::eof-or-lose stream eof-error-p eof-value))
			 (t
			  (sb-gray:stream-unread-char stream char)
			  char)))))
	     (t
	      (let ((char (sb-gray:stream-peek-char stream)))
		(if (eq char :eof)
		    (sb-impl::eof-or-lose stream eof-error-p eof-value)
		    char))))))))

(defun listen (&optional (stream *standard-input*) (width 1))
  "Returns T if Width octets are available on the given Stream.  If Width
  is given as 'character, check for a character."
  ;; WIDTH is number of octets which must be available; any value
  ;; other than 1 is treated as 'character.
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :input)
       (with-stream-class (simple-stream stream)
	 (if (not (eql width 1))
	     (funcall-stm-handler j-listen stream)
	     (or (< (sm buffpos stream) (sm buffer-ptr stream))
		 ;; Note: should try DEVICE-EXTEND for more on buffer streams
		 (when (>= (sm mode stream) 0) ;; device-connected
		   (incf (sm last-char-read-size stream))
		   (let ((ok (refill-buffer stream nil)))
		     (decf (sm last-char-read-size stream))
		     (plusp ok)))))))
      (ansi-stream
       (or (/= (the fixnum (sb-kernel:ansi-stream-in-index stream))
               sb-impl::+ansi-stream-in-buffer-length+)
	    ;; Test for T explicitly since misc methods return :EOF sometimes.
	    (eq (funcall (sb-kernel:ansi-stream-misc stream) stream :listen)
                t)))
      (fundamental-stream
       (sb-gray:stream-listen stream)))))

(declaim (inline %simple-stream-read-line))
(defun %simple-stream-read-line (stream eof-error-p eof-value)
  (declare (type simple-stream stream)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (simple-stream)
    (let* ((cbuf (make-string 80))	; current buffer
	   (bufs (list cbuf))		; list of buffers
	   (tail bufs)			; last cons of bufs list
	   (index 0)			; current index in current buffer
	   (total 0))			; total characters
      (declare (type simple-base-string cbuf)
	       (type cons bufs tail)
	       (type fixnum index total))
      (loop
	(multiple-value-bind (chars done)
	    (funcall-stm-handler j-read-chars stream cbuf
				 #\Newline index (length cbuf) t)
	  (declare (type fixnum chars))
	  (incf index chars)
	  (incf total chars)
	  (when (and (eq done :eof) (zerop index))
	    (if eof-error-p
		(error 'end-of-file :stream stream)
		(return (values eof-value t))))
	  (when done
	    ;; If there's only one buffer in use, return it directly
	    (when (null (cdr bufs))
	      (return (values (sb-kernel:shrink-vector cbuf index)
			      (eq done :eof))))
	    ;; If total fits in final buffer, use it
	    #-ignore
	    (when (<= total (length cbuf))
	      (replace cbuf cbuf :start1 (- total index) :end2 index)
	      (let ((idx 0))
		(declare (type fixnum idx))
		(dolist (buf bufs)
		  (declare (type simple-base-string buf))
		  (replace cbuf buf :start1 idx)
		  (incf idx (length buf))))
	      (return (values (sb-kernel:shrink-vector cbuf index)
			      (eq done :eof))))
	    ;; Allocate new string of appropriate length
	    (let ((string (make-string total))
		  (index 0))
	      (declare (type fixnum index))
	      (dolist (buf bufs)
		(declare (type simple-base-string buf))
		(replace string buf :start1 index)
		(incf index (length buf)))
	      (return  (values string (eq done :eof)))))
	  (when (>= index (length cbuf))
	    (setf cbuf (make-string (the fixnum (* 2 index))))
	    (setf index 0)
	    (setf (cdr tail) (cons cbuf nil))
	    (setf tail (cdr tail))))))))

(defun read-line (&optional (stream *standard-input*) (eof-error-p t)
			    eof-value recursive-p)
  "Returns a line of text read from the Stream as a string, discarding the
  newline character."
  (declare (ignore recursive-p))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :input)
       (%simple-stream-read-line stream eof-error-p eof-value))
      (ansi-stream
       (%ansi-stream-read-line stream eof-error-p eof-value))
      (fundamental-stream
       (multiple-value-bind (string eof) (sb-gray:stream-read-line stream)
	 (if (and eof (zerop (length string)))
	     (values (sb-impl::eof-or-lose stream eof-error-p eof-value) t)
	     (values string eof)))))))

(defun read-sequence (seq stream &key (start 0) (end nil) partial-fill)
  "Destructively modify SEQ by reading elements from STREAM.
  SEQ is bounded by START and END. SEQ is destructively modified by
  copying successive elements into it from STREAM. If the end of file
  for STREAM is reached before copying all elements of the subsequence,
  then the extra elements near the end of sequence are not updated, and
  the index of the next element is returned."
  (let ((stream (sb-impl::in-synonym-of stream))
	(end (or end (length seq))))
    (etypecase stream
      (simple-stream
       (with-stream-class (simple-stream stream)
	 (%check-simple-stream stream :input)
	 (etypecase seq
	   (string
	    (funcall-stm-handler j-read-chars stream seq nil start end
				 (if partial-fill :bnb t)))
	   ((or (simple-array (unsigned-byte 8) (*))
                (simple-array (signed-byte 8) (*)))
	    ;; TODO: "read-vector" equivalent, but blocking if
	    ;; partial-fill is NIL
	    (error "implement me")
	    ))))
      (ansi-stream
       (%ansi-stream-read-sequence seq stream start end))
      (fundamental-stream
       (sb-gray:stream-read-sequence seq stream start end)))))

(defun clear-input (&optional (stream *standard-input*) buffer-only)
  "Clears any buffered input associated with the Stream."
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (with-stream-class (simple-stream stream)
	 (%check-simple-stream stream :input)
	 (setf (sm buffpos stream) 0
	       (sm buffer-ptr stream) 0
	       (sm last-char-read-size stream) 0) ;; ??
	 (device-clear-input stream buffer-only)))
      (ansi-stream
       (setf (sb-kernel:ansi-stream-in-index stream)
             sb-impl::+ansi-stream-in-buffer-length+)
       (funcall (sb-kernel:ansi-stream-misc stream) stream :clear-input))
      (fundamental-stream
       (sb-gray:stream-clear-input stream))))
  nil)

(defun write-byte (integer stream)
  "Outputs an octet to the Stream."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (cond ((any-stream-instance-flags stream :string)
		(error "Can't write-byte on string streams"))
	       ((any-stream-instance-flags stream :dual)
		(let ((ptr (sm outpos stream)))
		  (when (>= ptr (sm max-out-pos stream))
		    (dc-flush-buffer stream t)
		    (setf ptr (1- (sm outpos stream))))
		  (setf (sm outpos stream) (1+ ptr))
		  (setf (bref (sm out-buffer stream) ptr) integer)))
	       (t  ;; single-channel-simple-stream
		(let ((ptr (sm buffpos stream)))
                  ;; FIXME: Shouldn't this be buf-len, not buffer-ptr?
		  (when (>= ptr (sm buffer-ptr stream))
		    (sc-flush-buffer stream t)
		    (setf ptr (1- (sm buffpos stream))))
		  (setf (sm buffpos stream) (1+ ptr))
		  (setf (bref (sm buffer stream) ptr) integer))))))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-bout stream) stream integer))
      (fundamental-stream
       (sb-gray:stream-write-byte stream integer))))
  integer)

(defun write-char (character &optional (stream *standard-output*))
  "Outputs the Character to the Stream."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (funcall-stm-handler-2 j-write-char character stream)))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-out stream) stream character))
      (fundamental-stream
       (sb-gray:stream-write-char stream character))))
  character)

(defun write-string (string &optional (stream *standard-output*)
			    &key (start 0) (end nil))
  "Outputs the String to the given Stream."
  (let ((stream (sb-impl::out-synonym-of stream))
	(end (or end (length string))))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (funcall-stm-handler-2 j-write-chars string stream start end))
       string)
      (ansi-stream
       (%ansi-stream-write-string string stream start end))
      (fundamental-stream
       (sb-gray:stream-write-string stream string start end)))))

(defun write-line (string &optional (stream *standard-output*)
			  &key (start 0) end)
  (declare (type string string))
  ;; FIXME: Why is there this difference between the treatments of the
  ;; STREAM argument in WRITE-STRING and WRITE-LINE?
  (let ((stream (sb-impl::out-synonym-of stream))
	(end (or end (length string))))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (funcall-stm-handler-2 j-write-chars string stream start end)
	 (funcall-stm-handler-2 j-write-char #\Newline stream)))
      (ansi-stream
       (%ansi-stream-write-string string stream start end)
       (funcall (sb-kernel:ansi-stream-out stream) stream #\Newline))
      (fundamental-stream
       (sb-gray:stream-write-string stream string start end)
       (sb-gray:stream-terpri stream))))
  string)

(defun write-sequence (seq stream &key (start 0) (end nil))
  "Write the elements of SEQ bounded by START and END to STREAM."
  (let ((stream (sb-impl::out-synonym-of stream))
	(end (or end (length seq))))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (etypecase seq
	   (string
	    (funcall-stm-handler-2 j-write-chars seq stream start end))
	   ((or (simple-array (unsigned-byte 8) (*))
                (simple-array (signed-byte 8) (*)))
	    ;; TODO: "write-vector" equivalent
	    (error "implement me")
	    ))))
      (ansi-stream
       (%ansi-stream-write-sequence seq stream start end))
      (fundamental-stream
       (sb-gray:stream-write-sequence seq stream start end)))))

(defun terpri (&optional (stream *standard-output*))
  "Outputs a new line to the Stream."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (funcall-stm-handler-2 j-write-char #\Newline stream)))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-out stream) stream #\Newline))
      (fundamental-stream
       (sb-gray:stream-terpri stream))))
  nil)

(defun fresh-line (&optional (stream *standard-output*))
  "Outputs a new line to the Stream if it is not positioned at the beginning of
   a line.  Returns T if it output a new line, nil otherwise."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (when (/= (or (sm charpos stream) 1) 0)
	   (funcall-stm-handler-2 j-write-char #\Newline stream)
	   t)))
      (ansi-stream
       (when (/= (or (sb-kernel:charpos stream) 1) 0)
	 (funcall (sb-kernel:ansi-stream-out stream) stream #\Newline)
	 t))
      (fundamental-stream
       (sb-gray:stream-fresh-line stream)))))

(defun finish-output (&optional (stream *standard-output*))
  "Attempts to ensure that all output sent to the Stream has reached its
   destination, and only then returns."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (cond ((any-stream-instance-flags stream :string)
		#| nothing to do |#)
	       ((any-stream-instance-flags stream :dual)
		(dc-flush-buffer stream t))
	       (t
		(sc-flush-buffer stream t)))))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :finish-output))
      (fundamental-stream
       (sb-gray:stream-finish-output stream))))
  nil)

(defun force-output (&optional (stream *standard-output*))
  "Attempts to force any buffered output to be sent."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 (cond ((any-stream-instance-flags stream :string)
		#| nothing to do |#)
	       ((any-stream-instance-flags stream :dual)
		(dc-flush-buffer stream nil))
	       (t
		(sc-flush-buffer stream nil)))))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :force-output))
      (fundamental-stream
       (sb-gray:stream-force-output stream))))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  "Clears the given output Stream."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream stream)
	 #| clear output buffer |#
	 (device-clear-output stream)))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :clear-output))
      (fundamental-stream
       (sb-gray:stream-clear-output stream))))
  nil)

(defun file-position (stream &optional position)
  "With one argument returns the current position within the file
   File-Stream is open to.  If the second argument is supplied, then
   this becomes the new file position.  The second argument may also
   be :start or :end for the start and end of the file, respectively."
  (etypecase stream
    (simple-stream
     (%check-simple-stream stream)
     (cond (position
	    ;; set unread to zero
	    ;; if position is within buffer, just move pointer; else
	    ;; flush output, if necessary
	    ;; set buffer pointer to 0, to force a read
	    (setf (device-file-position stream) position))
	   (t
	    (let ((posn (device-file-position stream)))
	      ;; adjust for buffer position
	      )))
     #| TODO: implement me |#)
    (ansi-stream
     (cond (position
	    (setf (sb-kernel:ansi-stream-in-index stream)
                  sb-impl::+ansi-stream-in-buffer-length+)
	    (funcall (sb-kernel:ansi-stream-misc stream)
		     stream :file-position position))
	   (t
	    (let ((res (funcall (sb-kernel:ansi-stream-misc stream)
				stream :file-position nil)))
	      (when res
		(- res
		   (- sb-impl::+ansi-stream-in-buffer-length+
		      (sb-kernel:ansi-stream-in-index stream))))))))
    (fundamental-stream
     (error "file-position not supported on Gray streams."))))

(defun file-length (stream)
  "This function returns the length of the file that File-Stream is open to."
  (etypecase stream
    (simple-stream
     (%check-simple-stream stream)
     (device-file-length stream)
     #| implement me |#)
    (ansi-stream
     (sb-impl::stream-must-be-associated-with-file stream)
     (funcall (sb-kernel:ansi-stream-misc stream) stream :file-length))
    (fundamental-stream
     (error "file-length not supported on Gray streams."))))

(defun line-length (&optional (stream *standard-output*))
  "Returns the number of characters that will fit on a line of output on the
  given Stream, or Nil if that information is not available."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       #| implement me |#)
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :line-length))
      (fundamental-stream
       (sb-gray:stream-line-length stream)))))

(defun charpos (&optional (stream *standard-output*))
  "Returns the number of characters on the current line of output of the given
  Stream, or Nil if that information is not availible."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       (with-stream-class (simple-stream) (sm charpos stream)))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :charpos))
      (fundamental-stream
       (sb-gray:stream-line-column stream)))))

(defun line-length (&optional (stream *standard-output*))
  "Returns the number of characters in a line of output of the given
  Stream, or Nil if that information is not availible."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check-simple-stream stream :output)
       ;; TODO (sat 2003-04-02): a way to specify a line length would
       ;; be good, I suppose.  Returning nil here means
       ;; sb-pretty::default-line-length is used.
       nil)
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :line-length))
      (fundamental-stream
       (sb-gray:stream-line-length stream)))))

(defun wait-for-input-available (stream &optional timeout)
  "Waits for input to become available on the Stream and returns T.  If
  Timeout expires, Nil is returned."
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (fixnum
       (sb-sys:wait-until-fd-usable stream :input timeout))
      (simple-stream
       (%check-simple-stream stream :input)
       (with-stream-class (simple-stream stream)
	 (or (< (sm buffpos stream) (sm buffer-ptr stream))
	     (wait-for-input-available (sm input-handle stream) timeout))))
      (two-way-stream
       (wait-for-input-available (two-way-stream-input-stream stream) timeout))
      (synonym-stream
       (wait-for-input-available (symbol-value (synonym-stream-symbol stream))
				 timeout))
      (sb-sys::file-stream
       (or (< (sb-impl::fd-stream-in-index stream)
	      (length (sb-impl::fd-stream-in-buffer stream)))
	   (wait-for-input-available (sb-sys:fd-stream-fd stream) timeout))))))

;;;
;;; SETUP
;;;

(defmethod shared-initialize :after ((instance simple-stream) slot-names
				     &rest initargs &allow-other-keys)
  (declare (ignore slot-names))
  (unless (slot-boundp instance 'melded-stream)
    (setf (slot-value instance 'melded-stream) instance)
    (setf (slot-value instance 'melding-base) instance))
  (unless (device-open instance initargs)
    (device-close instance t)))

;;; From the simple-streams documentation: "A generic function implies
;;; a specialization capability that does not exist for
;;; simple-streams; simple-stream specializations should be on
;;; device-close."  So don't do it.
(defmethod close ((stream simple-stream) &key abort)
  (device-close stream abort))


;;; bugfix
;;; sat 2003-01-12: What is this for?
#+nil
(defun cl::stream-misc-dispatch (stream operation &optional arg1 arg2)
  (declare (type fundamental-stream stream) ;; this is a lie
           (ignore arg2))
  (case operation
    (:listen
     (ext:stream-listen stream))
    (:unread
     (ext:stream-unread-char stream arg1))
    (:close
     (close stream))
    (:clear-input
     (ext:stream-clear-input stream))
    (:force-output
     (ext:stream-force-output stream))
    (:finish-output
     (ext:stream-finish-output stream))
    (:element-type
     (stream-element-type stream))
    (:interactive-p
     (interactive-stream-p stream))
    (:line-length
     (ext:stream-line-length stream))
    (:charpos
     (ext:stream-line-column stream))
    (:file-length
     (file-length stream))
    (:file-position
     (file-position stream arg1))))
