;;;; streams for UNIX file descriptors

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; buffer manipulation routines

;;; FIXME: Is it really good to maintain this pool separate from the
;;; GC and the C malloc logic?
(defvar *available-buffers* ()
  #!+sb-doc
  "List of available buffers. Each buffer is an sap pointing to
  bytes-per-buffer of memory.")

(defconstant bytes-per-buffer (* 4 1024)
  #!+sb-doc
  "Number of bytes per buffer.")

;;; Return the next available buffer, creating one if necessary.
#!-sb-fluid (declaim (inline next-available-buffer))
(defun next-available-buffer ()
  (if *available-buffers*
      (pop *available-buffers*)
      (allocate-system-memory bytes-per-buffer)))

;;;; the FD-STREAM structure

(defstruct (fd-stream
	    (:constructor %make-fd-stream)
	    (:conc-name fd-stream-)
	    (:predicate fd-stream-p)
	    (:include ansi-stream
		      (misc #'fd-stream-misc-routine))
	    (:copier nil))

  ;; the name of this stream
  (name nil)
  ;; the file this stream is for
  (file nil)
  ;; the backup file namestring for the old file, for :IF-EXISTS
  ;; :RENAME or :RENAME-AND-DELETE.
  (original nil :type (or simple-string null))
  (delete-original nil)	      ; for :if-exists :rename-and-delete
  ;;; the number of bytes per element
  (element-size 1 :type index)
  ;; the type of element being transfered
  (element-type 'base-char)   
  ;; the Unix file descriptor
  (fd -1 :type fixnum)	      
  ;; controls when the output buffer is flushed
  (buffering :full :type (member :full :line :none))
  ;; controls whether the input buffer must be cleared before output
  ;; (must be done for files, not for sockets, pipes and other data
  ;; sources where input and output aren't related).  non-NIL means
  ;; don't clear input buffer.
  (dual-channel-p nil)
  ;; character position (if known)
  (char-pos nil :type (or index null))
  ;; T if input is waiting on FD. :EOF if we hit EOF.
  (listen nil :type (member nil t :eof))

  ;; the input buffer
  (unread nil)
  (ibuf-sap nil :type (or system-area-pointer null))
  (ibuf-length nil :type (or index null))
  (ibuf-head 0 :type index)
  (ibuf-tail 0 :type index)

  ;; the output buffer
  (obuf-sap nil :type (or system-area-pointer null))
  (obuf-length nil :type (or index null))
  (obuf-tail 0 :type index)

  ;; output flushed, but not written due to non-blocking io?
  (output-later nil)
  (handler nil)
  ;; timeout specified for this stream, or NIL if none
  (timeout nil :type (or index null))
  ;; pathname of the file this stream is opened to (returned by PATHNAME)
  (pathname nil :type (or pathname null))
  (external-format :default)
  (output-bytes #'ill-out :type function))
(def!method print-object ((fd-stream fd-stream) stream)
  (declare (type stream stream))
  (print-unreadable-object (fd-stream stream :type t :identity t)
    (format stream "for ~S" (fd-stream-name fd-stream))))

;;;; output routines and related noise

(defvar *output-routines* ()
  #!+sb-doc
  "List of all available output routines. Each element is a list of the
  element-type output, the kind of buffering, the function name, and the number
  of bytes per element.")

;;; common idioms for reporting low-level stream and file problems
(defun simple-stream-perror (note-format stream errno)
  (error 'simple-stream-error
	 :stream stream
	 :format-control "~@<~?: ~2I~_~A~:>"
	 :format-arguments (list note-format (list stream) (strerror errno))))
(defun simple-file-perror (note-format pathname errno)
  (error 'simple-file-error
	 :pathname pathname
	 :format-control "~@<~?: ~2I~_~A~:>"
	 :format-arguments
	 (list note-format (list pathname) (strerror errno))))

(defun stream-decoding-error (stream octets)
  (error 'stream-decoding-error
	 :stream stream
         ;; FIXME: dunno how to get at OCTETS currently, or even if
         ;; that's the right thing to report.
         :octets octets))
(defun stream-encoding-error (stream code)
  (error 'stream-encoding-error
	 :stream stream
         :code code))

;;; Returning true goes into end of file handling, false will enter another
;;; round of input buffer filling followed by re-entering character decode.
(defun stream-decoding-error-and-handle (stream octet-count)
  (restart-case
      (stream-decoding-error stream
			     (let ((sap (fd-stream-ibuf-sap stream))
				   (head (fd-stream-ibuf-head stream)))
			       (loop for i from 0 below octet-count
				     collect (sap-ref-8 sap (+ head i)))))
    (attempt-resync ()
      :report (lambda (stream)
		(format stream
			"~@<Attempt to resync the stream at a character ~
                        character boundary and continue.~@:>"))
      (fd-stream-resync stream)
      nil)
    (force-end-of-file ()
      :report (lambda (stream)
		(format stream "~@<Force an end of file.~@:>"))
      t)))

(defun stream-encoding-error-and-handle (stream code)
  (restart-case
      (stream-encoding-error stream code)
    (output-nothing ()
      :report (lambda (stream)
		(format stream "~@<Skip output of this character.~@:>"))
      (throw 'output-nothing nil))))

;;; This is called by the server when we can write to the given file
;;; descriptor. Attempt to write the data again. If it worked, remove
;;; the data from the OUTPUT-LATER list. If it didn't work, something
;;; is wrong.
(defun frob-output-later (stream)
  (let* ((stuff (pop (fd-stream-output-later stream)))
	 (base (car stuff))
	 (start (cadr stuff))
	 (end (caddr stuff))
	 (reuse-sap (cadddr stuff))
	 (length (- end start)))
    (declare (type index start end length))
    (multiple-value-bind (count errno)
	(sb!unix:unix-write (fd-stream-fd stream)
			    base
			    start
			    length)
      (cond ((not count)
	     (if (= errno sb!unix:ewouldblock)
		 (error "Write would have blocked, but SERVER told us to go.")
		 (simple-stream-perror "couldn't write to ~S" stream errno)))
	    ((eql count length) ; Hot damn, it worked.
	     (when reuse-sap
	       (push base *available-buffers*)))
	    ((not (null count)) ; sorta worked..
	     (push (list base
			 (the index (+ start count))
			 end)
		   (fd-stream-output-later stream))))))
  (unless (fd-stream-output-later stream)
    (sb!sys:remove-fd-handler (fd-stream-handler stream))
    (setf (fd-stream-handler stream) nil)))

;;; Arange to output the string when we can write on the file descriptor.
(defun output-later (stream base start end reuse-sap)
  (cond ((null (fd-stream-output-later stream))
	 (setf (fd-stream-output-later stream)
	       (list (list base start end reuse-sap)))
	 (setf (fd-stream-handler stream)
	       (sb!sys:add-fd-handler (fd-stream-fd stream)
				      :output
				      (lambda (fd)
					(declare (ignore fd))
					(frob-output-later stream)))))
	(t
	 (nconc (fd-stream-output-later stream)
		(list (list base start end reuse-sap)))))
  (when reuse-sap
    (let ((new-buffer (next-available-buffer)))
      (setf (fd-stream-obuf-sap stream) new-buffer)
      (setf (fd-stream-obuf-length stream) bytes-per-buffer))))

;;; Output the given noise. Check to see whether there are any pending
;;; writes. If so, just queue this one. Otherwise, try to write it. If
;;; this would block, queue it.
(defun frob-output (stream base start end reuse-sap)
  (declare (type fd-stream stream)
	   (type (or system-area-pointer (simple-array * (*))) base)
	   (type index start end))
  (if (not (null (fd-stream-output-later stream))) ; something buffered.
      (progn
	(output-later stream base start end reuse-sap)
	;; ### check to see whether any of this noise can be output
	)
      (let ((length (- end start)))
	(multiple-value-bind (count errno)
	    (sb!unix:unix-write (fd-stream-fd stream) base start length)
	  (cond ((not count)
		 (if (= errno sb!unix:ewouldblock)
		     (output-later stream base start end reuse-sap)
		     (simple-stream-perror "couldn't write to ~S"
					   stream
					   errno)))
		((not (eql count length))
		 (output-later stream base (the index (+ start count))
			       end reuse-sap)))))))

;;; Flush any data in the output buffer.
(defun flush-output-buffer (stream)
  (let ((length (fd-stream-obuf-tail stream)))
    (unless (= length 0)
      (frob-output stream (fd-stream-obuf-sap stream) 0 length t)
      (setf (fd-stream-obuf-tail stream) 0))))

(defmacro output-wrapper/variable-width ((stream size buffering restart)
					 &body body)
  (let ((stream-var (gensym)))
    `(let ((,stream-var ,stream)
	   (size ,size))
      ,(unless (eq (car buffering) :none)
	 `(when (< (fd-stream-obuf-length ,stream-var)
	           (+ (fd-stream-obuf-tail ,stream-var)
		       size))
            (flush-output-buffer ,stream-var)))
      ,(unless (eq (car buffering) :none)
	 `(when (and (not (fd-stream-dual-channel-p ,stream-var))
		     (> (fd-stream-ibuf-tail ,stream-var)
			(fd-stream-ibuf-head ,stream-var)))
            (file-position ,stream-var (file-position ,stream-var))))
      ,(if restart
           `(catch 'output-nothing
	      ,@body
	      (incf (fd-stream-obuf-tail ,stream-var) size))
           `(progn
             ,@body
             (incf (fd-stream-obuf-tail ,stream-var) size)))
      ,(ecase (car buffering)
	 (:none
	  `(flush-output-buffer ,stream-var))
	 (:line
	  `(when (eq (char-code byte) (char-code #\Newline))
	     (flush-output-buffer ,stream-var)))
	 (:full))
    (values))))

(defmacro output-wrapper ((stream size buffering restart) &body body)
  (let ((stream-var (gensym)))
    `(let ((,stream-var ,stream))
      ,(unless (eq (car buffering) :none)
	 `(when (< (fd-stream-obuf-length ,stream-var)
	           (+ (fd-stream-obuf-tail ,stream-var)
		       ,size))
            (flush-output-buffer ,stream-var)))
      ,(unless (eq (car buffering) :none)
	 `(when (and (not (fd-stream-dual-channel-p ,stream-var))
		     (> (fd-stream-ibuf-tail ,stream-var)
			(fd-stream-ibuf-head ,stream-var)))
            (file-position ,stream-var (file-position ,stream-var))))
      ,(if restart
	   `(catch 'output-nothing
	      ,@body
	      (incf (fd-stream-obuf-tail ,stream-var) ,size))
           `(progn
             ,@body
             (incf (fd-stream-obuf-tail ,stream-var) ,size)))
      ,(ecase (car buffering)
	 (:none
	  `(flush-output-buffer ,stream-var))
	 (:line
	  `(when (eq (char-code byte) (char-code #\Newline))
	     (flush-output-buffer ,stream-var)))
	 (:full))
    (values))))

(defmacro def-output-routines/variable-width
    ((name-fmt size restart external-format &rest bufferings)
     &body body)
  (declare (optimize (speed 1)))
  (cons 'progn
	(mapcar
	    (lambda (buffering)
	      (let ((function
		     (intern (format nil name-fmt (string (car buffering))))))
		`(progn
		   (defun ,function (stream byte)
		     (output-wrapper/variable-width (stream ,size ,buffering ,restart)
		       ,@body))
		   (setf *output-routines*
			 (nconc *output-routines*
				',(mapcar
				   (lambda (type)
				     (list type
					   (car buffering)
					   function
					   1
					   external-format))
				   (cdr buffering)))))))
	    bufferings)))

;;; Define output routines that output numbers SIZE bytes long for the
;;; given bufferings. Use BODY to do the actual output.
(defmacro def-output-routines ((name-fmt size restart &rest bufferings)
                               &body body)
  (declare (optimize (speed 1)))
  (cons 'progn
	(mapcar
	    (lambda (buffering)
	      (let ((function
		     (intern (format nil name-fmt (string (car buffering))))))
		`(progn
		   (defun ,function (stream byte)
		     (output-wrapper (stream ,size ,buffering ,restart)
		       ,@body))
		   (setf *output-routines*
			 (nconc *output-routines*
				',(mapcar
				   (lambda (type)
				     (list type
					   (car buffering)
					   function
					   size
					   nil))
				   (cdr buffering)))))))
	    bufferings)))

;;; FIXME: is this used anywhere any more?
(def-output-routines ("OUTPUT-CHAR-~A-BUFFERED"
		      1
                      t
		      (:none character)
		      (:line character)
		      (:full character))
  (if (char= byte #\Newline)
      (setf (fd-stream-char-pos stream) 0)
      (incf (fd-stream-char-pos stream)))
  (setf (sap-ref-8 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	(char-code byte)))

(def-output-routines ("OUTPUT-UNSIGNED-BYTE-~A-BUFFERED"
		      1
                      nil
		      (:none (unsigned-byte 8))
		      (:full (unsigned-byte 8)))
  (setf (sap-ref-8 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-SIGNED-BYTE-~A-BUFFERED"
		      1
                      nil
		      (:none (signed-byte 8))
		      (:full (signed-byte 8)))
  (setf (signed-sap-ref-8 (fd-stream-obuf-sap stream)
			  (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-UNSIGNED-SHORT-~A-BUFFERED"
		      2
                      nil
		      (:none (unsigned-byte 16))
		      (:full (unsigned-byte 16)))
  (setf (sap-ref-16 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-SIGNED-SHORT-~A-BUFFERED"
		      2
                      nil
		      (:none (signed-byte 16))
		      (:full (signed-byte 16)))
  (setf (signed-sap-ref-16 (fd-stream-obuf-sap stream)
			   (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-UNSIGNED-LONG-~A-BUFFERED"
		      4
                      nil
		      (:none (unsigned-byte 32))
		      (:full (unsigned-byte 32)))
  (setf (sap-ref-32 (fd-stream-obuf-sap stream) (fd-stream-obuf-tail stream))
	byte))

(def-output-routines ("OUTPUT-SIGNED-LONG-~A-BUFFERED"
		      4
                      nil
		      (:none (signed-byte 32))
		      (:full (signed-byte 32)))
  (setf (signed-sap-ref-32 (fd-stream-obuf-sap stream)
			   (fd-stream-obuf-tail stream))
	byte))

;;; Do the actual output. If there is space to buffer the string,
;;; buffer it. If the string would normally fit in the buffer, but
;;; doesn't because of other stuff in the buffer, flush the old noise
;;; out of the buffer and put the string in it. Otherwise we have a
;;; very long string, so just send it directly (after flushing the
;;; buffer, of course).
(defun output-raw-bytes (fd-stream thing &optional start end)
  #!+sb-doc
  "Output THING to FD-STREAM. THING can be any kind of vector or a SAP. If
  THING is a SAP, END must be supplied (as length won't work)."
  (let ((start (or start 0))
	(end (or end (length (the (simple-array * (*)) thing)))))
    (declare (type index start end))
    (when (and (not (fd-stream-dual-channel-p fd-stream))
	       (> (fd-stream-ibuf-tail fd-stream)
		  (fd-stream-ibuf-head fd-stream)))
      (file-position fd-stream (file-position fd-stream)))
    (let* ((len (fd-stream-obuf-length fd-stream))
	   (tail (fd-stream-obuf-tail fd-stream))
	   (space (- len tail))
	   (bytes (- end start))
	   (newtail (+ tail bytes)))
      (cond ((minusp bytes) ; error case
	     (error ":END before :START!"))
	    ((zerop bytes)) ; easy case
	    ((<= bytes space)
	     (if (system-area-pointer-p thing)
		 (system-area-ub8-copy thing start
                                       (fd-stream-obuf-sap fd-stream)
                                       tail
                                       bytes)
		 ;; FIXME: There should be some type checking somewhere to
		 ;; verify that THING here is a vector, not just <not a SAP>.
		 (copy-ub8-to-system-area thing start
                                          (fd-stream-obuf-sap fd-stream)
                                          tail
                                          bytes))
	     (setf (fd-stream-obuf-tail fd-stream) newtail))
	    ((<= bytes len)
	     (flush-output-buffer fd-stream)
	     (if (system-area-pointer-p thing)
		 (system-area-ub8-copy thing
                                       start
                                       (fd-stream-obuf-sap fd-stream)
                                       0
                                       bytes)
		 ;; FIXME: There should be some type checking somewhere to
		 ;; verify that THING here is a vector, not just <not a SAP>.
		 (copy-ub8-to-system-area thing
                                          start
                                          (fd-stream-obuf-sap fd-stream)
                                          0
                                          bytes))
	     (setf (fd-stream-obuf-tail fd-stream) bytes))
	    (t
	     (flush-output-buffer fd-stream)
	     (frob-output fd-stream thing start end nil))))))

;;; the routine to use to output a string. If the stream is
;;; unbuffered, slam the string down the file descriptor, otherwise
;;; use OUTPUT-RAW-BYTES to buffer the string. Update charpos by
;;; checking to see where the last newline was.
;;;
;;; Note: some bozos (the FASL dumper) call write-string with things
;;; other than strings. Therefore, we must make sure we have a string
;;; before calling POSITION on it.
;;; KLUDGE: It would be better to fix the bozos instead of trying to
;;; cover for them here. -- WHN 20000203
(defun fd-sout (stream thing start end)
  (let ((start (or start 0))
	(end (or end (length (the vector thing)))))
    (declare (fixnum start end))
    (if (stringp thing)
	(let ((last-newline (and (find #\newline (the simple-string thing)
				       :start start :end end)
				 ;; FIXME why do we need both calls?
				 ;; Is find faster forwards than
				 ;; position is backwards?
				 (position #\newline (the simple-string thing)
					   :from-end t
					   :start start
					   :end end))))
	  (if (and (typep thing 'base-string)
		   (eq (fd-stream-external-format stream) :latin-1))
              (ecase (fd-stream-buffering stream)
                (:full
                 (output-raw-bytes stream thing start end))
                (:line
                 (output-raw-bytes stream thing start end)
                 (when last-newline
                   (flush-output-buffer stream)))
                (:none
                 (frob-output stream thing start end nil)))
	      (ecase (fd-stream-buffering stream)
		(:full (funcall (fd-stream-output-bytes stream)
				stream thing nil start end))
		(:line (funcall (fd-stream-output-bytes stream)
				stream thing last-newline start end))
		(:none (funcall (fd-stream-output-bytes stream)
				stream thing t start end))))
	  (if last-newline
	      (setf (fd-stream-char-pos stream)
		    (- end last-newline 1))
	      (incf (fd-stream-char-pos stream)
		    (- end start))))
	(ecase (fd-stream-buffering stream)
	  ((:line :full)
	   (output-raw-bytes stream thing start end))
	  (:none
	   (frob-output stream thing start end nil))))))

(defvar *external-formats* ()
  #!+sb-doc
  "List of all available external formats. Each element is a list of the
  element-type, string input function name, character input function name,
  and string output function name.")

;;; Find an output routine to use given the type and buffering. Return
;;; as multiple values the routine, the real type transfered, and the
;;; number of bytes per element.
(defun pick-output-routine (type buffering &optional external-format)
  (when (subtypep type 'character)
    (dolist (entry *external-formats*)
      (when (member external-format (first entry))
	(return-from pick-output-routine
	  (values (symbol-function (nth (ecase buffering
					  (:none 4)
					  (:line 5)
					  (:full 6))
					entry))
		  'character
		  1
		  (symbol-function (fourth entry))
		  (first (first entry)))))))
  (dolist (entry *output-routines*)
    (when (and (subtypep type (first entry))
	       (eq buffering (second entry))
	       (or (not (fifth entry))
		   (eq external-format (fifth entry))))
      (return-from pick-output-routine
	(values (symbol-function (third entry))
		(first entry)
		(fourth entry)))))
  ;; KLUDGE: dealing with the buffering here leads to excessive code
  ;; explosion.
  ;;
  ;; KLUDGE: also see comments in PICK-INPUT-ROUTINE
  (loop for i from 40 by 8 to 1024 ; ARB (KLUDGE)
	if (subtypep type `(unsigned-byte ,i))
	do (return-from pick-output-routine
	     (values
	      (ecase buffering
		(:none
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:none) nil)
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte))))))
		(:full
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:full) nil)
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte)))))))
	      `(unsigned-byte ,i)
	      (/ i 8))))
  (loop for i from 40 by 8 to 1024 ; ARB (KLUDGE)
	if (subtypep type `(signed-byte ,i))
	do (return-from pick-output-routine
	     (values
	      (ecase buffering
		(:none
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:none) nil)
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte))))))
		(:full
		 (lambda (stream byte)
		   (output-wrapper (stream (/ i 8) (:full) nil)
		     (loop for j from 0 below (/ i 8)
			   do (setf (sap-ref-8 
				     (fd-stream-obuf-sap stream)
				     (+ j (fd-stream-obuf-tail stream)))
				    (ldb (byte 8 (- i 8 (* j 8))) byte)))))))
	      `(signed-byte ,i)
	      (/ i 8)))))

;;;; input routines and related noise

;;; a list of all available input routines. Each element is a list of
;;; the element-type input, the function name, and the number of bytes
;;; per element.
(defvar *input-routines* ())

;;; Fill the input buffer, and return the number of bytes read. Throw
;;; to EOF-INPUT-CATCHER if the eof was reached. Drop into
;;; SYSTEM:SERVER if necessary.
(defun refill-buffer/fd (stream)
  (let ((fd (fd-stream-fd stream))
	(ibuf-sap (fd-stream-ibuf-sap stream))
	(buflen (fd-stream-ibuf-length stream))
	(head (fd-stream-ibuf-head stream))
	(tail (fd-stream-ibuf-tail stream)))
    (declare (type index head tail))
    (unless (zerop head)
      (cond ((eql head tail)
	     (setf head 0)
	     (setf tail 0)
	     (setf (fd-stream-ibuf-head stream) 0)
	     (setf (fd-stream-ibuf-tail stream) 0))
	    (t
	     (decf tail head)
	     (system-area-ub8-copy ibuf-sap head
                                   ibuf-sap 0 tail)
	     (setf head 0)
	     (setf (fd-stream-ibuf-head stream) 0)
	     (setf (fd-stream-ibuf-tail stream) tail))))
    (setf (fd-stream-listen stream) nil)
    (multiple-value-bind (count errno)
	;; FIXME: Judging from compiler warnings, this WITH-ALIEN form expands
	;; into something which uses the not-yet-defined type
	;;   (SB!ALIEN-INTERNALS:ALIEN (* (SB!ALIEN:STRUCT SB!UNIX:FD-SET))).
	;; This is probably inefficient and unsafe and generally bad, so
	;; try to find some way to make that type known before
	;; this is compiled.
	(sb!alien:with-alien ((read-fds (sb!alien:struct sb!unix:fd-set)))
	  (sb!unix:fd-zero read-fds)
	  (sb!unix:fd-set fd read-fds)
	  (sb!unix:unix-fast-select (1+ fd)
				    (sb!alien:addr read-fds)
				    nil
				    nil
				    0
				    0))
      (case count
	(1)
	(0
	 (unless (sb!sys:wait-until-fd-usable
		  fd :input (fd-stream-timeout stream))
	   (error 'io-timeout :stream stream :direction :read)))
	(t
	 (simple-stream-perror "couldn't check whether ~S is readable"
			       stream
			       errno))))
    (multiple-value-bind (count errno)
	(sb!unix:unix-read fd
			   (sb!sys:int-sap (+ (sb!sys:sap-int ibuf-sap) tail))
			   (- buflen tail))
      (cond ((null count)
	     (if (eql errno sb!unix:ewouldblock)
		 (progn
		   (unless (sb!sys:wait-until-fd-usable
			    fd :input (fd-stream-timeout stream))
		     (error 'io-timeout :stream stream :direction :read))
		   (refill-buffer/fd stream))
		 (simple-stream-perror "couldn't read from ~S" stream errno)))
	    ((zerop count)
	     (setf (fd-stream-listen stream) :eof)
	     (/show0 "THROWing EOF-INPUT-CATCHER")
	     (throw 'eof-input-catcher nil))
	    (t
	     (incf (fd-stream-ibuf-tail stream) count)
             count)))))
			
;;; Make sure there are at least BYTES number of bytes in the input
;;; buffer. Keep calling REFILL-BUFFER/FD until that condition is met.
(defmacro input-at-least (stream bytes)
  (let ((stream-var (gensym))
	(bytes-var (gensym)))
    `(let ((,stream-var ,stream)
	   (,bytes-var ,bytes))
       (loop
	 (when (>= (- (fd-stream-ibuf-tail ,stream-var)
		      (fd-stream-ibuf-head ,stream-var))
		   ,bytes-var)
	   (return))
	 (refill-buffer/fd ,stream-var)))))

(defmacro input-wrapper/variable-width ((stream bytes eof-error eof-value)
					&body read-forms)
  (let ((stream-var (gensym))
	(retry-var (gensym))
	(element-var (gensym)))
    `(let ((,stream-var ,stream)
	   (size nil))
       (if (fd-stream-unread ,stream-var)
	   (prog1
	       (fd-stream-unread ,stream-var)
	     (setf (fd-stream-unread ,stream-var) nil)
	     (setf (fd-stream-listen ,stream-var) nil))
	   (let ((,element-var nil)
		 (decode-break-reason nil))
	     (do ((,retry-var t))
		 ((not ,retry-var))
	       (unless
		   (catch 'eof-input-catcher
		     (setf decode-break-reason
			   (block decode-break-reason
			     (input-at-least ,stream-var 1)
			     (let* ((byte (sap-ref-8 (fd-stream-ibuf-sap
						      ,stream-var)
						     (fd-stream-ibuf-head
						      ,stream-var))))
			       (setq size ,bytes)
			       (input-at-least ,stream-var size)
			       (setq ,element-var (locally ,@read-forms))
			       (setq ,retry-var nil))
			     nil))
		     (when decode-break-reason
		       (stream-decoding-error-and-handle stream
							 decode-break-reason))
		     t)
		 (let ((octet-count (- (fd-stream-ibuf-tail ,stream-var)
				      (fd-stream-ibuf-head ,stream-var))))
		   (when (or (zerop octet-count)
			     (and (not ,element-var)
				  (not decode-break-reason)
				  (stream-decoding-error-and-handle
				   stream octet-count)))
		     (setq ,retry-var nil)))))
	     (cond (,element-var
		    (incf (fd-stream-ibuf-head ,stream-var) size)
		    ,element-var)
		   (t
		    (eof-or-lose ,stream-var ,eof-error ,eof-value))))))))

;;; a macro to wrap around all input routines to handle EOF-ERROR noise
(defmacro input-wrapper ((stream bytes eof-error eof-value) &body read-forms)
  (let ((stream-var (gensym))
	(element-var (gensym)))
    `(let ((,stream-var ,stream))
       (if (fd-stream-unread ,stream-var)
	   (prog1
	       (fd-stream-unread ,stream-var)
	     (setf (fd-stream-unread ,stream-var) nil)
	     (setf (fd-stream-listen ,stream-var) nil))
	   (let ((,element-var
		  (catch 'eof-input-catcher
		    (input-at-least ,stream-var ,bytes)
		    (locally ,@read-forms))))
	     (cond (,element-var
		    (incf (fd-stream-ibuf-head ,stream-var) ,bytes)
		    ,element-var)
		   (t
		    (eof-or-lose ,stream-var ,eof-error ,eof-value))))))))

(defmacro def-input-routine/variable-width (name
					    (type external-format size sap head)
					    &rest body)
  `(progn
     (defun ,name (stream eof-error eof-value)
       (input-wrapper/variable-width (stream ,size eof-error eof-value)
	 (let ((,sap (fd-stream-ibuf-sap stream))
	       (,head (fd-stream-ibuf-head stream)))
	   ,@body)))
     (setf *input-routines*
	   (nconc *input-routines*
		  (list (list ',type ',name 1 ',external-format))))))

(defmacro def-input-routine (name
			     (type size sap head)
			     &rest body)
  `(progn
     (defun ,name (stream eof-error eof-value)
       (input-wrapper (stream ,size eof-error eof-value)
	 (let ((,sap (fd-stream-ibuf-sap stream))
	       (,head (fd-stream-ibuf-head stream)))
	   ,@body)))
     (setf *input-routines*
	   (nconc *input-routines*
		  (list (list ',type ',name ',size nil))))))

;;; STREAM-IN routine for reading a string char
(def-input-routine input-character
		   (character 1 sap head)
  (code-char (sap-ref-8 sap head)))

;;; STREAM-IN routine for reading an unsigned 8 bit number
(def-input-routine input-unsigned-8bit-byte
		   ((unsigned-byte 8) 1 sap head)
  (sap-ref-8 sap head))

;;; STREAM-IN routine for reading a signed 8 bit number
(def-input-routine input-signed-8bit-number
		   ((signed-byte 8) 1 sap head)
  (signed-sap-ref-8 sap head))

;;; STREAM-IN routine for reading an unsigned 16 bit number
(def-input-routine input-unsigned-16bit-byte
		   ((unsigned-byte 16) 2 sap head)
  (sap-ref-16 sap head))

;;; STREAM-IN routine for reading a signed 16 bit number
(def-input-routine input-signed-16bit-byte
		   ((signed-byte 16) 2 sap head)
  (signed-sap-ref-16 sap head))

;;; STREAM-IN routine for reading a unsigned 32 bit number
(def-input-routine input-unsigned-32bit-byte
		   ((unsigned-byte 32) 4 sap head)
  (sap-ref-32 sap head))

;;; STREAM-IN routine for reading a signed 32 bit number
(def-input-routine input-signed-32bit-byte
		   ((signed-byte 32) 4 sap head)
  (signed-sap-ref-32 sap head))



;;; Find an input routine to use given the type. Return as multiple
;;; values the routine, the real type transfered, and the number of
;;; bytes per element (and for character types string input routine).
(defun pick-input-routine (type &optional external-format)
  (when (subtypep type 'character)
    (dolist (entry *external-formats*)
      (when (member external-format (first entry))
	(return-from pick-input-routine
	  (values (symbol-function (third entry))
		  'character
		  1
		  (symbol-function (second entry))
		  (first (first entry)))))))
  (dolist (entry *input-routines*)
    (when (and (subtypep type (first entry))
	       (or (not (fourth entry))
		   (eq external-format (fourth entry))))
      (return-from pick-input-routine
	(values (symbol-function (second entry))
		(first entry)
		(third entry)))))
  ;; FIXME: let's do it the hard way, then (but ignore things like
  ;; endianness, efficiency, and the necessary coupling between these
  ;; and the output routines).  -- CSR, 2004-02-09
  (loop for i from 40 by 8 to 1024 ; ARB (well, KLUDGE really)
	if (subtypep type `(unsigned-byte ,i))
	do (return-from pick-input-routine
	     (values
	      (lambda (stream eof-error eof-value)
		(input-wrapper (stream (/ i 8) eof-error eof-value)
		  (let ((sap (fd-stream-ibuf-sap stream))
			(head (fd-stream-ibuf-head stream)))
		    (loop for j from 0 below (/ i 8)
			  with result = 0
			  do (setf result
				   (+ (* 256 result)
				      (sap-ref-8 sap (+ head j))))
			  finally (return result)))))
	      `(unsigned-byte ,i)
	      (/ i 8))))
  (loop for i from 40 by 8 to 1024 ; ARB (well, KLUDGE really)
	if (subtypep type `(signed-byte ,i))
	do (return-from pick-input-routine
	     (values
	      (lambda (stream eof-error eof-value)
		(input-wrapper (stream (/ i 8) eof-error eof-value)
		  (let ((sap (fd-stream-ibuf-sap stream))
			(head (fd-stream-ibuf-head stream)))
		    (loop for j from 0 below (/ i 8)
			  with result = 0
			  do (setf result
				   (+ (* 256 result)
				      (sap-ref-8 sap (+ head j))))
			  finally (return (if (logbitp (1- i) result)
                                              (dpb result (byte i 0) -1)
                                              result))))))
	      `(signed-byte ,i)
	      (/ i 8)))))

;;; Return a string constructed from SAP, START, and END.
(defun string-from-sap (sap start end)
  (declare (type index start end))
  (let* ((length (- end start))
	 (string (make-string length)))
    (copy-ub8-from-system-area sap start
                               string 0
                               length)
    string))

;;; the N-BIN method for FD-STREAMs
;;;
;;; Note that this blocks in UNIX-READ. It is generally used where
;;; there is a definite amount of reading to be done, so blocking
;;; isn't too problematical.
(defun fd-stream-read-n-bytes (stream buffer start requested eof-error-p
			       &aux (total-copied 0))
  (declare (type fd-stream stream))
  (declare (type index start requested total-copied))
  (let ((unread (fd-stream-unread stream)))
    (when unread
      ;; AVERs designed to fail when we have more complicated
      ;; character representations.
      (aver (typep unread 'base-char))
      (aver (= (fd-stream-element-size stream) 1))
      ;; KLUDGE: this is a slightly-unrolled-and-inlined version of
      ;; %BYTE-BLT
      (etypecase buffer
	(system-area-pointer
	 (setf (sap-ref-8 buffer start) (char-code unread)))
	((simple-unboxed-array (*))
	 (setf (aref buffer start) unread)))
      (setf (fd-stream-unread stream) nil)
      (setf (fd-stream-listen stream) nil)
      (incf total-copied)))
  (do ()
      (nil)
    (let* ((remaining-request (- requested total-copied))
	   (head (fd-stream-ibuf-head stream))
	   (tail (fd-stream-ibuf-tail stream))
	   (available (- tail head))
	   (n-this-copy (min remaining-request available))
	   (this-start (+ start total-copied))
	   (this-end (+ this-start n-this-copy))
	   (sap (fd-stream-ibuf-sap stream)))
      (declare (type index remaining-request head tail available))
      (declare (type index n-this-copy))
      ;; Copy data from stream buffer into user's buffer. 
      (%byte-blt sap head buffer this-start this-end)
      (incf (fd-stream-ibuf-head stream) n-this-copy)
      (incf total-copied n-this-copy)
      ;; Maybe we need to refill the stream buffer.
      (cond (;; If there were enough data in the stream buffer, we're done.
	     (= total-copied requested)
	     (return total-copied))
	    (;; If EOF, we're done in another way.
             (null (catch 'eof-input-catcher (refill-buffer/fd stream)))
	     (if eof-error-p
		 (error 'end-of-file :stream stream)
		 (return total-copied)))
	    ;; Otherwise we refilled the stream buffer, so fall
	    ;; through into another pass of the loop.
	    ))))

(defun fd-stream-resync (stream)
  (dolist (entry *external-formats*)
    (when (member (fd-stream-external-format stream) (first entry))
      (return-from fd-stream-resync
	(funcall (symbol-function (eighth entry)) stream)))))

;;; FIXME: OAOOM here vrt. *EXTERNAL-FORMAT-FUNCTIONS* in fd-stream.lisp
(defmacro define-external-format (external-format size output-restart
                                  out-expr in-expr)
  (let* ((name (first external-format))
         (out-function (symbolicate "OUTPUT-BYTES/" name))
         (format (format nil "OUTPUT-CHAR-~A-~~A-BUFFERED" (string name)))
         (in-function (symbolicate "FD-STREAM-READ-N-CHARACTERS/" name))
         (in-char-function (symbolicate "INPUT-CHAR/" name)))
    `(progn
      (defun ,out-function (stream string flush-p start end)
	(let ((start (or start 0))
	      (end (or end (length string))))
	  (declare (type index start end))
	  (when (and (not (fd-stream-dual-channel-p stream))
		     (> (fd-stream-ibuf-tail stream)
			(fd-stream-ibuf-head stream)))
	    (file-position stream (file-position stream)))
	  (when (< end start)
	    (error ":END before :START!"))
	  (do ()
	      ((= end start))
	    (setf (fd-stream-obuf-tail stream)
		  (do* ((len (fd-stream-obuf-length stream))
			(sap (fd-stream-obuf-sap stream))
			(tail (fd-stream-obuf-tail stream)))
		       ((or (= start end) (< (- len tail) 4)) tail)
                    ,(if output-restart
			 `(catch 'output-nothing
			    (let* ((byte (aref string start))
				   (bits (char-code byte)))
			      ,out-expr
			      (incf tail ,size)))
                         `(let* ((byte (aref string start))
                                  (bits (char-code byte)))
                             ,out-expr
                             (incf tail ,size)))
		    (incf start)))
	    (when (< start end)
	      (flush-output-buffer stream)))
	  (when flush-p
	    (flush-output-buffer stream))))
      (def-output-routines (,format
			    ,size
                            ,output-restart
			    (:none character)
			    (:line character)
			    (:full character))
	  (if (char= byte #\Newline)
	      (setf (fd-stream-char-pos stream) 0)
	      (incf (fd-stream-char-pos stream)))
	(let ((bits (char-code byte))
	      (sap (fd-stream-obuf-sap stream))
	      (tail (fd-stream-obuf-tail stream)))
	  ,out-expr))
      (defun ,in-function (stream buffer start requested eof-error-p
			   &aux (total-copied 0))
	(declare (type fd-stream stream))
	(declare (type index start requested total-copied))
	(let ((unread (fd-stream-unread stream)))
	  (when unread
	    (setf (aref buffer start) unread)
	    (setf (fd-stream-unread stream) nil)
	    (setf (fd-stream-listen stream) nil)
	    (incf total-copied)))
	(do ()
	    (nil)
	  (let* ((head (fd-stream-ibuf-head stream))
		 (tail (fd-stream-ibuf-tail stream))
		 (sap (fd-stream-ibuf-sap stream)))
	    (declare (type index head tail))
	    ;; Copy data from stream buffer into user's buffer.
	    (do ()
		((or (= tail head) (= requested total-copied)))
	      (let* ((byte (sap-ref-8 sap head)))
		(when (> ,size (- tail head))
		  (return))
		(setf (aref buffer (+ start total-copied)) ,in-expr)
		(incf total-copied)
		(incf head ,size)))
	    (setf (fd-stream-ibuf-head stream) head)
	    ;; Maybe we need to refill the stream buffer.
	    (cond ( ;; If there were enough data in the stream buffer, we're done.
		   (= total-copied requested)
		   (return total-copied))
		  ( ;; If EOF, we're done in another way.
                   (null (catch 'eof-input-catcher (refill-buffer/fd stream)))
		   (if eof-error-p
		       (error 'end-of-file :stream stream)
		       (return total-copied)))
		  ;; Otherwise we refilled the stream buffer, so fall
		  ;; through into another pass of the loop.
		  ))))
      (def-input-routine ,in-char-function (character ,size sap head)
	(let ((byte (sap-ref-8 sap head)))
	  ,in-expr))
      (setf *external-formats*
       (cons '(,external-format ,in-function ,in-char-function ,out-function
	       ,@(mapcar #'(lambda (buffering)
			     (intern (format nil format (string buffering))))
			 '(:none :line :full)))
	*external-formats*)))))

(defmacro define-external-format/variable-width
    (external-format output-restart out-size-expr
     out-expr in-size-expr in-expr)
  (let* ((name (first external-format))
	 (out-function (symbolicate "OUTPUT-BYTES/" name))
	 (format (format nil "OUTPUT-CHAR-~A-~~A-BUFFERED" (string name)))
	 (in-function (symbolicate "FD-STREAM-READ-N-CHARACTERS/" name))
	 (in-char-function (symbolicate "INPUT-CHAR/" name))
	 (resync-function (symbolicate "RESYNC/" name)))
    `(progn
      (defun ,out-function (stream string flush-p start end)
	(let ((start (or start 0))
	      (end (or end (length string))))
	  (declare (type index start end))
	  (when (and (not (fd-stream-dual-channel-p stream))
		     (> (fd-stream-ibuf-tail stream)
			(fd-stream-ibuf-head stream)))
	    (file-position stream (file-position stream)))
	  (when (< end start)
	    (error ":END before :START!"))
	  (do ()
	      ((= end start))
	    (setf (fd-stream-obuf-tail stream)
		  (do* ((len (fd-stream-obuf-length stream))
			(sap (fd-stream-obuf-sap stream))
			(tail (fd-stream-obuf-tail stream)))
		       ((or (= start end) (< (- len tail) 4)) tail)
		    ,(if output-restart
			 `(catch 'output-nothing
			    (let* ((byte (aref string start))
				   (bits (char-code byte))
				   (size ,out-size-expr))
			      ,out-expr
			      (incf tail size)))
			 `(let* ((byte (aref string start))
				 (bits (char-code byte))
				 (size ,out-size-expr))
			    ,out-expr
			    (incf tail size)))
		    (incf start)))
	    (when (< start end)
	      (flush-output-buffer stream)))
	  (when flush-p
	    (flush-output-buffer stream))))
      (def-output-routines/variable-width (,format
					   ,out-size-expr
                                           ,output-restart
					   ,external-format
					   (:none character)
					   (:line character)
					   (:full character))
	  (if (char= byte #\Newline)
	      (setf (fd-stream-char-pos stream) 0)
	      (incf (fd-stream-char-pos stream)))
	(let ((bits (char-code byte))
	      (sap (fd-stream-obuf-sap stream))
	      (tail (fd-stream-obuf-tail stream)))
	  ,out-expr))
      (defun ,in-function (stream buffer start requested eof-error-p
			   &aux (total-copied 0))
	(declare (type fd-stream stream))
	(declare (type index start requested total-copied))
	(let ((unread (fd-stream-unread stream)))
	  (when unread
	    (setf (aref buffer start) unread)
	    (setf (fd-stream-unread stream) nil)
	    (setf (fd-stream-listen stream) nil)
	    (incf total-copied)))
	(do ()
	    (nil)
	  (let* ((head (fd-stream-ibuf-head stream))
		 (tail (fd-stream-ibuf-tail stream))
		 (sap (fd-stream-ibuf-sap stream))
		 (head-start head)
		 (decode-break-reason nil))
	    (declare (type index head tail))
	    ;; Copy data from stream buffer into user's buffer.
	    (do ((size nil nil))
		((or (= tail head) (= requested total-copied)))
	      (setf decode-break-reason
		    (block decode-break-reason
		      (let ((byte (sap-ref-8 sap head)))
			(setq size ,in-size-expr)
			(when (> size (- tail head))
			  (return))
			(setf (aref buffer (+ start total-copied)) ,in-expr)
			(incf total-copied)
			(incf head size))
		      nil))
	      (setf (fd-stream-ibuf-head stream) head)
	      (when (and decode-break-reason
			 (= head head-start))
		(when (stream-decoding-error-and-handle
		       stream decode-break-reason)
		  (if eof-error-p
		      (error 'end-of-file :stream stream)
		      (return-from ,in-function total-copied)))
		(setf head (fd-stream-ibuf-head stream))
		(setf tail (fd-stream-ibuf-tail stream)))
	      (when (plusp total-copied)
		(return-from ,in-function total-copied)))
	    (setf (fd-stream-ibuf-head stream) head)
	    ;; Maybe we need to refill the stream buffer.
	    (cond ( ;; If there were enough data in the stream buffer, we're done.
		   (= total-copied requested)
		   (return total-copied))
		  ( ;; If EOF, we're done in another way.
		   (or (eq decode-break-reason 'eof)
                       (null (catch 'eof-input-catcher 
                               (refill-buffer/fd stream))))
		   (if eof-error-p
		       (error 'end-of-file :stream stream)
		       (return total-copied)))
		  ;; Otherwise we refilled the stream buffer, so fall
		  ;; through into another pass of the loop.
		  ))))
      (def-input-routine/variable-width ,in-char-function (character
							   ,external-format
							   ,in-size-expr
							   sap head)
	(let ((byte (sap-ref-8 sap head)))
	  ,in-expr))
      (defun ,resync-function (stream)
        (loop (input-at-least stream 1)
              (incf (fd-stream-ibuf-head stream))
              (unless (block decode-break-reason
			(let* ((sap (fd-stream-ibuf-sap stream))
			       (head (fd-stream-ibuf-head stream))
			       (byte (sap-ref-8 sap head))
			       (size ,in-size-expr))
			  ,in-expr)
			nil)
                (return))))
      (setf *external-formats*
       (cons '(,external-format ,in-function ,in-char-function ,out-function
	       ,@(mapcar #'(lambda (buffering)
			     (intern (format nil format (string buffering))))
			 '(:none :line :full))
	       ,resync-function)
	*external-formats*)))))

(define-external-format (:latin-1 :latin1 :iso-8859-1)
    1 t
  (if (>= bits 256)
      (stream-encoding-error-and-handle stream bits)
      (setf (sap-ref-8 sap tail) bits))
  (code-char byte))

(define-external-format (:ascii :us-ascii :ansi_x3.4-1968 
                         :iso-646 :iso-646-us :|646|)
    1 t
  (if (>= bits 128)
      (stream-encoding-error-and-handle stream bits)
      (setf (sap-ref-8 sap tail) bits))
  (code-char byte))

(let* ((table (let ((s (make-string 256)))
		(map-into s #'code-char
			  '(#x00 #x01 #x02 #x03 #x9c #x09 #x86 #x7f #x97 #x8d #x8e #x0b #x0c #x0d #x0e #x0f
			    #x10 #x11 #x12 #x13 #x9d #x85 #x08 #x87 #x18 #x19 #x92 #x8f #x1c #x1d #x1e #x1f
			    #x80 #x81 #x82 #x83 #x84 #x0a #x17 #x1b #x88 #x89 #x8a #x8b #x8c #x05 #x06 #x07
			    #x90 #x91 #x16 #x93 #x94 #x95 #x96 #x04 #x98 #x99 #x9a #x9b #x14 #x15 #x9e #x1a
			    #x20 #xa0 #xe2 #xe4 #xe0 #xe1 #xe3 #xe5 #xe7 #xf1 #xa2 #x2e #x3c #x28 #x2b #x7c
			    #x26 #xe9 #xea #xeb #xe8 #xed #xee #xef #xec #xdf #x21 #x24 #x2a #x29 #x3b #xac
			    #x2d #x2f #xc2 #xc4 #xc0 #xc1 #xc3 #xc5 #xc7 #xd1 #xa6 #x2c #x25 #x5f #x3e #x3f
			    #xf8 #xc9 #xca #xcb #xc8 #xcd #xce #xcf #xcc #x60 #x3a #x23 #x40 #x27 #x3d #x22
			    #xd8 #x61 #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 #xab #xbb #xf0 #xfd #xfe #xb1
			    #xb0 #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 #x72 #xaa #xba #xe6 #xb8 #xc6 #xa4
			    #xb5 #x7e #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7a #xa1 #xbf #xd0 #xdd #xde #xae
			    #x5e #xa3 #xa5 #xb7 #xa9 #xa7 #xb6 #xbc #xbd #xbe #x5b #x5d #xaf #xa8 #xb4 #xd7
			    #x7b #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 #x49 #xad #xf4 #xf6 #xf2 #xf3 #xf5
			    #x7d #x4a #x4b #x4c #x4d #x4e #x4f #x50 #x51 #x52 #xb9 #xfb #xfc #xf9 #xfa #xff
			    #x5c #xf7 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a #xb2 #xd4 #xd6 #xd2 #xd3 #xd5
			    #x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39 #xb3 #xdb #xdc #xd9 #xda #x9f))
		s))
       (reverse-table (let ((rt (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
			  (loop for char across table for i from 0
			       do (aver (= 0 (aref rt (char-code char))))
			       do (setf (aref rt (char-code char)) i))
			  rt)))
  (define-external-format (:ebcdic-us :ibm-037 :ibm037)
      1 t
    (if (>= bits 256)
	(stream-encoding-error-and-handle stream bits)
	(setf (sap-ref-8 sap tail) (aref reverse-table bits)))
    (aref table byte)))
    

#!+sb-unicode
(let ((latin-9-table (let ((table (make-string 256)))
                       (do ((i 0 (1+ i)))
                           ((= i 256))
                         (setf (aref table i) (code-char i)))
                       (setf (aref table #xa4) (code-char #x20ac))
                       (setf (aref table #xa6) (code-char #x0160))
                       (setf (aref table #xa8) (code-char #x0161))
                       (setf (aref table #xb4) (code-char #x017d))
                       (setf (aref table #xb8) (code-char #x017e))
                       (setf (aref table #xbc) (code-char #x0152))
                       (setf (aref table #xbd) (code-char #x0153))
                       (setf (aref table #xbe) (code-char #x0178))
                       table))
      (latin-9-reverse-1 (make-array 16
                                     :element-type '(unsigned-byte 21)
                                     :initial-contents '(#x0160 #x0161 #x0152 #x0153 0 0 0 0 #x0178 0 0 0 #x20ac #x017d #x017e 0)))
      (latin-9-reverse-2 (make-array 16
                                     :element-type '(unsigned-byte 8)
                                     :initial-contents '(#xa6 #xa8 #xbc #xbd 0 0 0 0 #xbe 0 0 0 #xa4 #xb4 #xb8 0))))
  (define-external-format (:latin-9 :latin9 :iso-8859-15)
      1 t
    (setf (sap-ref-8 sap tail)
          (if (< bits 256)
              (if (= bits (char-code (aref latin-9-table bits)))
                  bits
                  (stream-encoding-error-and-handle stream byte))
              (if (= (aref latin-9-reverse-1 (logand bits 15)) bits)
                  (aref latin-9-reverse-2 (logand bits 15))
                  (stream-encoding-error-and-handle stream byte))))
    (aref latin-9-table byte)))

(define-external-format/variable-width (:utf-8 :utf8) nil
  (let ((bits (char-code byte)))
    (cond ((< bits #x80) 1)
	  ((< bits #x800) 2)
	  ((< bits #x10000) 3)
	  (t 4)))
  (ecase size
    (1 (setf (sap-ref-8 sap tail) bits))
    (2 (setf (sap-ref-8 sap tail) (logior #xc0 (ldb (byte 5 6) bits))
	     (sap-ref-8 sap (1+ tail)) (logior #x80 (ldb (byte 6 0) bits))))
    (3 (setf (sap-ref-8 sap tail) (logior #xe0 (ldb (byte 4 12) bits))
	     (sap-ref-8 sap (1+ tail)) (logior #x80 (ldb (byte 6 6) bits))
	     (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 0) bits))))
    (4 (setf (sap-ref-8 sap tail) (logior #xf0 (ldb (byte 3 18) bits))
	     (sap-ref-8 sap (1+ tail)) (logior #x80 (ldb (byte 6 12) bits))
	     (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 6) bits))
	     (sap-ref-8 sap (+ 3 tail)) (logior #x80 (ldb (byte 6 0) bits)))))
  (cond ((< byte #x80) 1)
	((< byte #xc2) (return-from decode-break-reason 1))
	((< byte #xe0) 2)
	((< byte #xf0) 3)
	(t 4))
  (code-char (ecase size
	       (1 byte)
	       (2 (let ((byte2 (sap-ref-8 sap (1+ head))))
		    (unless (<= #x80 byte2 #xbf)
		      (return-from decode-break-reason 2))
		    (dpb byte (byte 5 6) byte2)))
	       (3 (let ((byte2 (sap-ref-8 sap (1+ head)))
			(byte3 (sap-ref-8 sap (+ 2 head))))
		    (unless (and (<= #x80 byte2 #xbf)
				 (<= #x80 byte3 #xbf))
		      (return-from decode-break-reason 3))
		    (dpb byte (byte 4 12) (dpb byte2 (byte 6 6) byte3))))
	       (4 (let ((byte2 (sap-ref-8 sap (1+ head)))
			(byte3 (sap-ref-8 sap (+ 2 head)))
			(byte4 (sap-ref-8 sap (+ 3 head))))
		    (unless (and (<= #x80 byte2 #xbf)
				 (<= #x80 byte3 #xbf)
				 (<= #x80 byte4 #xbf))
		      (return-from decode-break-reason 4))
		    (dpb byte (byte 3 18)
			 (dpb byte2 (byte 6 12)
			      (dpb byte3 (byte 6 6) byte4))))))))

;;;; utility functions (misc routines, etc)

;;; Fill in the various routine slots for the given type. INPUT-P and
;;; OUTPUT-P indicate what slots to fill. The buffering slot must be
;;; set prior to calling this routine.
(defun set-fd-stream-routines (fd-stream element-type external-format
			       input-p output-p buffer-p)
  (let* ((target-type (case element-type
			(unsigned-byte '(unsigned-byte 8))
			(signed-byte '(signed-byte 8))
			(:default 'character)
			(t element-type)))
	 (character-stream-p (subtypep target-type 'character))
	 (bivalent-stream-p (eq element-type :default))
	 normalized-external-format
	 (bin-routine #'ill-bin)
	 (bin-type nil)
	 (bin-size nil)
	 (cin-routine #'ill-in)
	 (cin-type nil)
	 (cin-size nil)
	 (input-type nil)	    ;calculated from bin-type/cin-type
	 (input-size nil)	    ;calculated from bin-size/cin-size
	 (read-n-characters #'ill-in)
	 (bout-routine #'ill-bout)
	 (bout-type nil)
	 (bout-size nil)
	 (cout-routine #'ill-out)
	 (cout-type nil)
	 (cout-size nil)
	 (output-type nil)
	 (output-size nil)
	 (output-bytes #'ill-bout))

    ;; drop buffers when direction changes
    (when (and (fd-stream-obuf-sap fd-stream) (not output-p))
      (push (fd-stream-obuf-sap fd-stream) *available-buffers*)
      (setf (fd-stream-obuf-sap fd-stream) nil))
    (when (and (fd-stream-ibuf-sap fd-stream) (not input-p))
      (push (fd-stream-ibuf-sap fd-stream) *available-buffers*)
      (setf (fd-stream-ibuf-sap fd-stream) nil))
    (when input-p
      (setf (fd-stream-ibuf-sap fd-stream) (next-available-buffer))
      (setf (fd-stream-ibuf-length fd-stream) bytes-per-buffer)
      (setf (fd-stream-ibuf-tail fd-stream) 0))
    (when output-p
      (setf (fd-stream-obuf-sap fd-stream) (next-available-buffer))
      (setf (fd-stream-obuf-length fd-stream) bytes-per-buffer)
      (setf (fd-stream-obuf-tail fd-stream) 0)
      (setf (fd-stream-char-pos fd-stream) 0))

    (when (and character-stream-p
	       (eq external-format :default))
      (/show0 "/getting default external format")
      (setf external-format (default-external-format))
      (/show0 "cold-printing defaulted external-format:")
      #!+sb-show
      (cold-print external-format)
      (/show0 "matching to known aliases")
      (dolist (entry *external-formats*
		     (restart-case
                         (error "Invalid external-format ~A" 
                                external-format)
		      (use-default ()
                        :report "Set external format to LATIN-1"
                        (setf external-format :latin-1))))
        (/show0 "cold printing known aliases:")
        #!+sb-show
        (dolist (alias (first entry)) (cold-print alias))
        (/show0 "done cold-printing known aliases")
	(when (member external-format (first entry))
          (/show0 "matched")
	  (return)))
      (/show0 "/default external format ok"))
    
    (when input-p
      (when (or (not character-stream-p) bivalent-stream-p)
	(multiple-value-setq (bin-routine bin-type bin-size read-n-characters
					  normalized-external-format)
	  (pick-input-routine (if bivalent-stream-p '(unsigned-byte 8)
				  target-type)
			      external-format))
	(unless bin-routine
	  (error "could not find any input routine for ~S" target-type)))
      (when character-stream-p
	(multiple-value-setq (cin-routine cin-type cin-size read-n-characters
					  normalized-external-format)
	  (pick-input-routine target-type external-format))
	(unless cin-routine
	  (error "could not find any input routine for ~S" target-type)))      
      (setf (fd-stream-in fd-stream) cin-routine
	    (fd-stream-bin fd-stream) bin-routine)
      ;; character type gets preferential treatment
      (setf input-size (or cin-size bin-size))
      (setf input-type (or cin-type bin-type))
      (when normalized-external-format
	(setf (fd-stream-external-format fd-stream)
	      normalized-external-format))
      (when (= (or cin-size 1) (or bin-size 1) 1)
	(setf (fd-stream-n-bin fd-stream) ;XXX
	      (if (and character-stream-p (not bivalent-stream-p))
		  read-n-characters
		  #'fd-stream-read-n-bytes))
	;; Sometimes turn on fast-read-char/fast-read-byte.  Switch on
	;; for character and (unsigned-byte 8) streams.  In these
	;; cases, fast-read-* will read from the
	;; ansi-stream-(c)in-buffer, saving function calls.
	;; Otherwise, the various data-reading functions in the stream
	;; structure will be called.
	(when (and buffer-p
		   (not bivalent-stream-p)
		   ;; temporary disable on :io streams
		   (not output-p))
	  (cond (character-stream-p 
		 (setf (ansi-stream-cin-buffer fd-stream)
		       (make-array +ansi-stream-in-buffer-length+
				   :element-type 'character)))
		((equal target-type '(unsigned-byte 8))
		 (setf (ansi-stream-in-buffer fd-stream)
		       (make-array +ansi-stream-in-buffer-length+
				   :element-type '(unsigned-byte 8))))))))

    (when output-p
      (when (or (not character-stream-p) bivalent-stream-p)
	(multiple-value-setq (bout-routine bout-type bout-size output-bytes
					   normalized-external-format)
	  (pick-output-routine (if bivalent-stream-p
				   '(unsigned-byte 8)
				   target-type)
			       (fd-stream-buffering fd-stream)
			       external-format))
	(unless bout-routine
	  (error "could not find any output routine for ~S buffered ~S"
		 (fd-stream-buffering fd-stream)
		 target-type)))
      (when character-stream-p
	(multiple-value-setq (cout-routine cout-type cout-size output-bytes
					   normalized-external-format)
	  (pick-output-routine target-type
			       (fd-stream-buffering fd-stream)
			       external-format))
	(unless cout-routine
	  (error "could not find any output routine for ~S buffered ~S"
		 (fd-stream-buffering fd-stream)
		 target-type)))
      (when normalized-external-format
	(setf (fd-stream-external-format fd-stream)
	      normalized-external-format))
      (when character-stream-p
	(setf (fd-stream-output-bytes fd-stream) output-bytes))
      (setf (fd-stream-out fd-stream) cout-routine
	    (fd-stream-bout fd-stream) bout-routine
	    (fd-stream-sout fd-stream) (if (eql cout-size 1)
					   #'fd-sout #'ill-out))
      (setf output-size (or cout-size bout-size))
      (setf output-type (or cout-type bout-type)))

    (when (and input-size output-size
	       (not (eq input-size output-size)))
      (error "Element sizes for input (~S:~S) and output (~S:~S) differ?"
	     input-type input-size
	     output-type output-size))
    (setf (fd-stream-element-size fd-stream)
	  (or input-size output-size))

    (setf (fd-stream-element-type fd-stream)
	  (cond ((equal input-type output-type)
		 input-type)
		((null output-type)
		 input-type)
		((null input-type)
		 output-type)
		((subtypep input-type output-type)
		 input-type)
		((subtypep output-type input-type)
		 output-type)
		(t
		 (error "Input type (~S) and output type (~S) are unrelated?"
			input-type
			output-type))))))

;;; Handle miscellaneous operations on FD-STREAM.
(defun fd-stream-misc-routine (fd-stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:listen
     (or (not (eql (fd-stream-ibuf-head fd-stream)
		   (fd-stream-ibuf-tail fd-stream)))
	 (fd-stream-listen fd-stream)
	 (setf (fd-stream-listen fd-stream)
	       (eql (sb!alien:with-alien ((read-fds (sb!alien:struct
						     sb!unix:fd-set)))
		      (sb!unix:fd-zero read-fds)
		      (sb!unix:fd-set (fd-stream-fd fd-stream) read-fds)
		      (sb!unix:unix-fast-select (1+ (fd-stream-fd fd-stream))
						(sb!alien:addr read-fds)
						nil nil 0 0))
		    1))))
    (:unread
     (setf (fd-stream-unread fd-stream) arg1)
     (setf (fd-stream-listen fd-stream) t))
    (:close
     (cond (arg1 ; We got us an abort on our hands.
	    (when (fd-stream-handler fd-stream)
	      (sb!sys:remove-fd-handler (fd-stream-handler fd-stream))
	      (setf (fd-stream-handler fd-stream) nil))
	    ;; We can't do anything unless we know what file were
	    ;; dealing with, and we don't want to do anything
	    ;; strange unless we were writing to the file.
	    (when (and (fd-stream-file fd-stream)
		       (fd-stream-obuf-sap fd-stream))
	      (if (fd-stream-original fd-stream)
		  ;; If the original is EQ to file we are appending
		  ;; and can just close the file without renaming.
		  (unless (eq (fd-stream-original fd-stream)
			      (fd-stream-file fd-stream))
		    ;; We have a handle on the original, just revert.
		    (multiple-value-bind (okay err)
			(sb!unix:unix-rename (fd-stream-original fd-stream)
					     (fd-stream-file fd-stream))
		      (unless okay
			(simple-stream-perror
			 "couldn't restore ~S to its original contents"
			 fd-stream
			 err))))
		  ;; We can't restore the original, and aren't
		  ;; appending, so nuke that puppy.
		  ;;
		  ;; FIXME: This is currently the fate of superseded
		  ;; files, and according to the CLOSE spec this is
		  ;; wrong. However, there seems to be no clean way to
		  ;; do that that doesn't involve either copying the
		  ;; data (bad if the :abort resulted from a full
		  ;; disk), or renaming the old file temporarily
		  ;; (probably bad because stream opening becomes more
		  ;; racy).
		  (multiple-value-bind (okay err)
		      (sb!unix:unix-unlink (fd-stream-file fd-stream))
		    (unless okay
		      (error 'simple-file-error
			     :pathname (fd-stream-file fd-stream)
			     :format-control
			     "~@<couldn't remove ~S: ~2I~_~A~:>"
			     :format-arguments (list (fd-stream-file fd-stream)
						     (strerror err))))))))
	   (t
	    (fd-stream-misc-routine fd-stream :finish-output)
	    (when (and (fd-stream-original fd-stream)
		       (fd-stream-delete-original fd-stream))
	      (multiple-value-bind (okay err)
		  (sb!unix:unix-unlink (fd-stream-original fd-stream))
		(unless okay
		  (error 'simple-file-error
			 :pathname (fd-stream-original fd-stream)
			 :format-control 
			 "~@<couldn't delete ~S during close of ~S: ~
                          ~2I~_~A~:>"
			 :format-arguments
			 (list (fd-stream-original fd-stream)
			       fd-stream
			       (strerror err))))))))
     (when (fboundp 'cancel-finalization)
       (cancel-finalization fd-stream))
     (sb!unix:unix-close (fd-stream-fd fd-stream))
     (when (fd-stream-obuf-sap fd-stream)
       (push (fd-stream-obuf-sap fd-stream) *available-buffers*)
       (setf (fd-stream-obuf-sap fd-stream) nil))
     (when (fd-stream-ibuf-sap fd-stream)
       (push (fd-stream-ibuf-sap fd-stream) *available-buffers*)
       (setf (fd-stream-ibuf-sap fd-stream) nil))
     (sb!impl::set-closed-flame fd-stream))
    (:clear-input
     (setf (fd-stream-unread fd-stream) nil)
     (setf (fd-stream-ibuf-head fd-stream) 0)
     (setf (fd-stream-ibuf-tail fd-stream) 0)
     (catch 'eof-input-catcher
       (loop
	(let ((count (sb!alien:with-alien ((read-fds (sb!alien:struct
						      sb!unix:fd-set)))
		       (sb!unix:fd-zero read-fds)
		       (sb!unix:fd-set (fd-stream-fd fd-stream) read-fds)
		       (sb!unix:unix-fast-select (1+ (fd-stream-fd fd-stream))
						 (sb!alien:addr read-fds)
						 nil
						 nil
						 0
						 0))))
	  (cond ((eql count 1)
		 (refill-buffer/fd fd-stream)
		 (setf (fd-stream-ibuf-head fd-stream) 0)
		 (setf (fd-stream-ibuf-tail fd-stream) 0))
		(t
		 (return t)))))))
    (:force-output
     (flush-output-buffer fd-stream))
    (:finish-output
     (flush-output-buffer fd-stream)
     (do ()
	 ((null (fd-stream-output-later fd-stream)))
       (sb!sys:serve-all-events)))
    (:element-type
     (fd-stream-element-type fd-stream))
    (:external-format
     (fd-stream-external-format fd-stream))
    (:interactive-p
     (= 1 (the (member 0 1)
            (sb!unix:unix-isatty (fd-stream-fd fd-stream)))))
    (:line-length
     80)
    (:charpos
     (fd-stream-char-pos fd-stream))
    (:file-length
     (unless (fd-stream-file fd-stream)
       ;; This is a TYPE-ERROR because ANSI's species FILE-LENGTH
       ;; "should signal an error of type TYPE-ERROR if stream is not
       ;; a stream associated with a file". Too bad there's no very
       ;; appropriate value for the EXPECTED-TYPE slot..
       (error 'simple-type-error
              :datum fd-stream
              :expected-type 'fd-stream
              :format-control "~S is not a stream associated with a file."
              :format-arguments (list fd-stream)))
     (multiple-value-bind (okay dev ino mode nlink uid gid rdev size
			   atime mtime ctime blksize blocks)
	 (sb!unix:unix-fstat (fd-stream-fd fd-stream))
       (declare (ignore ino nlink uid gid rdev
			atime mtime ctime blksize blocks))
       (unless okay
	 (simple-stream-perror "failed Unix fstat(2) on ~S" fd-stream dev))
       (if (zerop mode)
	   nil
	   (truncate size (fd-stream-element-size fd-stream)))))
    ;; FIXME: I doubt this is correct in the presence of Unicode,
    ;; since fd-stream FILE-POSITION is measured in bytes. 
    (:file-string-length
     (etypecase arg1
       (character 1)
       (string (length arg1))))
    (:file-position
     (fd-stream-file-position fd-stream arg1))))

(defun fd-stream-file-position (stream &optional newpos)
  (declare (type fd-stream stream)
	   (type (or (alien sb!unix:off-t) (member nil :start :end)) newpos))
  (if (null newpos)
      (sb!sys:without-interrupts
	;; First, find the position of the UNIX file descriptor in the file.
	(multiple-value-bind (posn errno)
	    (sb!unix:unix-lseek (fd-stream-fd stream) 0 sb!unix:l_incr)
	  (declare (type (or (alien sb!unix:off-t) null) posn))
	  (cond ((integerp posn)
		 ;; Adjust for buffered output: If there is any output
		 ;; buffered, the *real* file position will be larger
		 ;; than reported by lseek() because lseek() obviously
		 ;; cannot take into account output we have not sent
		 ;; yet.
		 (dolist (later (fd-stream-output-later stream))
		   (incf posn (- (caddr later)
				 (cadr later))))
		 (incf posn (fd-stream-obuf-tail stream))
		 ;; Adjust for unread input: If there is any input
		 ;; read from UNIX but not supplied to the user of the
		 ;; stream, the *real* file position will smaller than
		 ;; reported, because we want to look like the unread
		 ;; stuff is still available.
		 (decf posn (- (fd-stream-ibuf-tail stream)
			       (fd-stream-ibuf-head stream)))
		 (when (fd-stream-unread stream)
		   (decf posn))
		 ;; Divide bytes by element size.
		 (truncate posn (fd-stream-element-size stream)))
		((eq errno sb!unix:espipe)
		 nil)
		(t
		 (sb!sys:with-interrupts
		   (simple-stream-perror "failure in Unix lseek() on ~S"
					 stream
					 errno))))))
      (let ((offset 0) origin)
	(declare (type (alien sb!unix:off-t) offset))
	;; Make sure we don't have any output pending, because if we
	;; move the file pointer before writing this stuff, it will be
	;; written in the wrong location.
	(flush-output-buffer stream)
	(do ()
	    ((null (fd-stream-output-later stream)))
	  (sb!sys:serve-all-events))
	;; Clear out any pending input to force the next read to go to
	;; the disk.
	(setf (fd-stream-unread stream) nil)
	(setf (fd-stream-ibuf-head stream) 0)
	(setf (fd-stream-ibuf-tail stream) 0)
	;; Trash cached value for listen, so that we check next time.
	(setf (fd-stream-listen stream) nil)
	;; Now move it.
	(cond ((eq newpos :start)
	       (setf offset 0 origin sb!unix:l_set))
	      ((eq newpos :end)
	       (setf offset 0 origin sb!unix:l_xtnd))
	      ((typep newpos '(alien sb!unix:off-t))
	       (setf offset (* newpos (fd-stream-element-size stream))
		     origin sb!unix:l_set))
	      (t
	       (error "invalid position given to FILE-POSITION: ~S" newpos)))
	(multiple-value-bind (posn errno)
	    (sb!unix:unix-lseek (fd-stream-fd stream) offset origin)
	  (cond ((typep posn '(alien sb!unix:off-t))
		 t)
		((eq errno sb!unix:espipe)
		 nil)
		(t
		 (simple-stream-perror "error in Unix lseek() on ~S"
				       stream
				       errno)))))))

;;;; creation routines (MAKE-FD-STREAM and OPEN)

;;; Create a stream for the given Unix file descriptor.
;;;
;;; If INPUT is non-NIL, allow input operations. If OUTPUT is non-nil,
;;; allow output operations. If neither INPUT nor OUTPUT is specified,
;;; default to allowing input.
;;;
;;; ELEMENT-TYPE indicates the element type to use (as for OPEN).
;;;
;;; BUFFERING indicates the kind of buffering to use.
;;;
;;; TIMEOUT (if true) is the number of seconds to wait for input. If
;;; NIL (the default), then wait forever. When we time out, we signal
;;; IO-TIMEOUT.
;;;
;;; FILE is the name of the file (will be returned by PATHNAME).
;;;
;;; NAME is used to identify the stream when printed.
(defun make-fd-stream (fd
		       &key
		       (input nil input-p)
		       (output nil output-p)
		       (element-type 'base-char)
		       (buffering :full)
		       (external-format :default)
		       timeout
		       file
		       original
		       delete-original
		       pathname
		       input-buffer-p
		       dual-channel-p
		       (name (if file
				 (format nil "file ~A" file)
				 (format nil "descriptor ~W" fd)))
		       auto-close)
  (declare (type index fd) (type (or index null) timeout)
	   (type (member :none :line :full) buffering))
  (cond ((not (or input-p output-p))
	 (setf input t))
	((not (or input output))
	 (error "File descriptor must be opened either for input or output.")))
  (let ((stream (%make-fd-stream :fd fd
				 :name name
				 :file file
				 :original original
				 :delete-original delete-original
				 :pathname pathname
				 :buffering buffering
				 :dual-channel-p dual-channel-p
				 :external-format external-format
				 :timeout timeout)))
    (set-fd-stream-routines stream element-type external-format
			    input output input-buffer-p)
    (when (and auto-close (fboundp 'finalize))
      (finalize stream
		(lambda ()
		  (sb!unix:unix-close fd)
		  #!+sb-show
		  (format *terminal-io* "** closed file descriptor ~W **~%"
			  fd))))
    stream))

;;; Pick a name to use for the backup file for the :IF-EXISTS
;;; :RENAME-AND-DELETE and :RENAME options.
(defun pick-backup-name (name)
  (declare (type simple-base-string name))
  (concatenate 'simple-base-string name ".bak"))

;;; Ensure that the given arg is one of the given list of valid
;;; things. Allow the user to fix any problems.
(defun ensure-one-of (item list what)
  (unless (member item list)
    (error 'simple-type-error
	   :datum item
	   :expected-type `(member ,@list)
	   :format-control "~@<~S is ~_invalid for ~S; ~_need one of~{ ~S~}~:>"
	   :format-arguments (list item what list))))

;;; Rename NAMESTRING to ORIGINAL. First, check whether we have write
;;; access, since we don't want to trash unwritable files even if we
;;; technically can. We return true if we succeed in renaming.
(defun rename-the-old-one (namestring original)
  (unless (sb!unix:unix-access namestring sb!unix:w_ok)
    (error "~@<The file ~2I~_~S ~I~_is not writable.~:>" namestring))
  (multiple-value-bind (okay err) (sb!unix:unix-rename namestring original)
    (if okay
	t
	(error 'simple-file-error
	       :pathname namestring
	       :format-control 
	       "~@<couldn't rename ~2I~_~S ~I~_to ~2I~_~S: ~4I~_~A~:>"
	       :format-arguments (list namestring original (strerror err))))))

(defun open (filename
	     &key
	     (direction :input)
	     (element-type 'base-char)
	     (if-exists nil if-exists-given)
	     (if-does-not-exist nil if-does-not-exist-given)
	     (external-format :default)
	     &aux ; Squelch assignment warning.
	     (direction direction)
	     (if-does-not-exist if-does-not-exist)
	     (if-exists if-exists))
  #!+sb-doc
  "Return a stream which reads from or writes to FILENAME.
  Defined keywords:
   :DIRECTION - one of :INPUT, :OUTPUT, :IO, or :PROBE
   :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR
   :IF-EXISTS - one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
                       :OVERWRITE, :APPEND, :SUPERSEDE or NIL
   :IF-DOES-NOT-EXIST - one of :ERROR, :CREATE or NIL
  See the manual for details."

  ;; Calculate useful stuff.
  (multiple-value-bind (input output mask)
      (case direction
	(:input  (values   t nil sb!unix:o_rdonly))
	(:output (values nil   t sb!unix:o_wronly))
	(:io     (values   t   t sb!unix:o_rdwr))
	(:probe  (values   t nil sb!unix:o_rdonly)))
    (declare (type index mask))
    (let* ((pathname (merge-pathnames filename))
	   (namestring
	    (cond ((unix-namestring pathname input))
		  ((and input (eq if-does-not-exist :create))
		   (unix-namestring pathname nil))
		  ((and (eq direction :io) (not if-does-not-exist-given))
		   (unix-namestring pathname nil)))))
      ;; Process if-exists argument if we are doing any output.
      (cond (output
	     (unless if-exists-given
	       (setf if-exists
		     (if (eq (pathname-version pathname) :newest)
			 :new-version
			 :error)))
	     (ensure-one-of if-exists
			    '(:error :new-version :rename
				     :rename-and-delete :overwrite
				     :append :supersede nil)
			    :if-exists)
	     (case if-exists
	       ((:new-version :error nil)
		(setf mask (logior mask sb!unix:o_excl)))
	       ((:rename :rename-and-delete)
		(setf mask (logior mask sb!unix:o_creat)))
	       ((:supersede)
		(setf mask (logior mask sb!unix:o_trunc)))
	       (:append
		(setf mask (logior mask sb!unix:o_append)))))
	    (t
	     (setf if-exists :ignore-this-arg)))

      (unless if-does-not-exist-given
	(setf if-does-not-exist
	      (cond ((eq direction :input) :error)
		    ((and output
			  (member if-exists '(:overwrite :append)))
		     :error)
		    ((eq direction :probe)
		     nil)
		    (t
		     :create))))
      (ensure-one-of if-does-not-exist
		     '(:error :create nil)
		     :if-does-not-exist)
      (if (eq if-does-not-exist :create)
	(setf mask (logior mask sb!unix:o_creat)))

      (let ((original (case if-exists
			((:rename :rename-and-delete)
			 (pick-backup-name namestring))
			((:append :overwrite)
			 ;; KLUDGE: Provent CLOSE from deleting
			 ;; appending streams when called with :ABORT T
			 namestring)))
	    (delete-original (eq if-exists :rename-and-delete))
	    (mode #o666))
	(when (and original (not (eq original namestring)))
	  ;; We are doing a :RENAME or :RENAME-AND-DELETE. Determine
	  ;; whether the file already exists, make sure the original
	  ;; file is not a directory, and keep the mode.
	  (let ((exists
		 (and namestring
		      (multiple-value-bind (okay err/dev inode orig-mode)
			  (sb!unix:unix-stat namestring)
			(declare (ignore inode)
				 (type (or index null) orig-mode))
			(cond
			 (okay
			  (when (and output (= (logand orig-mode #o170000)
					       #o40000))
			    (error 'simple-file-error
				   :pathname namestring
				   :format-control
				   "can't open ~S for output: is a directory"
				   :format-arguments (list namestring)))
			  (setf mode (logand orig-mode #o777))
			  t)
			 ((eql err/dev sb!unix:enoent)
			  nil)
			 (t
			  (simple-file-perror "can't find ~S"
					      namestring
					      err/dev)))))))
	    (unless (and exists
			 (rename-the-old-one namestring original))
	      (setf original nil)
	      (setf delete-original nil)
	      ;; In order to use :SUPERSEDE instead, we have to make
	      ;; sure SB!UNIX:O_CREAT corresponds to
	      ;; IF-DOES-NOT-EXIST. SB!UNIX:O_CREAT was set before
	      ;; because of IF-EXISTS being :RENAME.
	      (unless (eq if-does-not-exist :create)
		(setf mask
		      (logior (logandc2 mask sb!unix:o_creat)
			      sb!unix:o_trunc)))
	      (setf if-exists :supersede))))

	;; Now we can try the actual Unix open(2).
	(multiple-value-bind (fd errno)
	    (if namestring
		(sb!unix:unix-open namestring mask mode)
		(values nil sb!unix:enoent))
	  (labels ((open-error (format-control &rest format-arguments)
		     (error 'simple-file-error
			    :pathname pathname
			    :format-control format-control
			    :format-arguments format-arguments))
		   (vanilla-open-error ()
		     (simple-file-perror "error opening ~S" pathname errno)))
	    (cond ((numberp fd)
		   (case direction
		     ((:input :output :io)
		      (make-fd-stream fd
				      :input input
				      :output output
				      :element-type element-type
				      :external-format external-format
				      :file namestring
				      :original original
				      :delete-original delete-original
				      :pathname pathname
				      :dual-channel-p nil
				      :input-buffer-p t
				      :auto-close t))
		     (:probe
		      (let ((stream
			     (%make-fd-stream :name namestring
					      :fd fd
					      :pathname pathname
					      :element-type element-type)))
			(close stream)
			stream))))
		  ((eql errno sb!unix:enoent)
		   (case if-does-not-exist
		     (:error (vanilla-open-error))
		     (:create
		      (open-error "~@<The path ~2I~_~S ~I~_does not exist.~:>"
				  pathname))
		     (t nil)))
		  ((and (eql errno sb!unix:eexist) (null if-exists))
		   nil)
		  (t
		   (vanilla-open-error)))))))))

;;;; initialization

;;; the stream connected to the controlling terminal, or NIL if there is none
(defvar *tty*)

;;; the stream connected to the standard input (file descriptor 0)
(defvar *stdin*)

;;; the stream connected to the standard output (file descriptor 1)
(defvar *stdout*)

;;; the stream connected to the standard error output (file descriptor 2)
(defvar *stderr*)

;;; This is called when the cold load is first started up, and may also
;;; be called in an attempt to recover from nested errors.
(defun stream-cold-init-or-reset ()
  (stream-reinit)
  (setf *terminal-io* (make-synonym-stream '*tty*))
  (setf *standard-output* (make-synonym-stream '*stdout*))
  (setf *standard-input* (make-synonym-stream '*stdin*))
  (setf *error-output* (make-synonym-stream '*stderr*))
  (setf *query-io* (make-synonym-stream '*terminal-io*))
  (setf *debug-io* *query-io*)
  (setf *trace-output* *standard-output*)
  (values))

;;; This is called whenever a saved core is restarted.
(defun stream-reinit ()
  (setf *available-buffers* nil)
  (setf *stdin*
	(make-fd-stream 0 :name "standard input" :input t :buffering :line))
  (setf *stdout*
	(make-fd-stream 1 :name "standard output" :output t :buffering :line))
  (setf *stderr*
	(make-fd-stream 2 :name "standard error" :output t :buffering :line))
  (let* ((ttyname #.(coerce "/dev/tty" 'simple-base-string))
	 (tty (sb!unix:unix-open ttyname sb!unix:o_rdwr #o666)))
    (if tty
	(setf *tty*
	      (make-fd-stream tty
			      :name "the terminal"
			      :input t
			      :output t
			      :buffering :line
			      :auto-close t))
	(setf *tty* (make-two-way-stream *stdin* *stdout*))))
  (values))

;;;; miscellany

;;; the Unix way to beep
(defun beep (stream)
  (write-char (code-char bell-char-code) stream)
  (finish-output stream))

;;; This is kind of like FILE-POSITION, but is an internal hack used
;;; by the filesys stuff to get and set the file name.
;;;
;;; FIXME: misleading name, screwy interface
(defun file-name (stream &optional new-name)
  (when (typep stream 'fd-stream)
      (cond (new-name
	     (setf (fd-stream-pathname stream) new-name)
	     (setf (fd-stream-file stream)
		   (unix-namestring new-name nil))
	     t)
	    (t
	     (fd-stream-pathname stream)))))
