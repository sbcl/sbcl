;;; -*- lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

(in-package "SB-SIMPLE-STREAMS")

;;;
;;; Stream printing
;;;

(defmethod print-object ((object file-simple-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "for ~S" (slot-value object 'filename))))

(defun make-control-table (&rest inits)
  (let ((table (make-array 32 :initial-element nil)))
    (do* ((char (pop inits) (pop inits))
	  (func (pop inits) (pop inits)))
	 ((null char))
      (when (< (char-code char) 32)
	(setf (aref table (char-code char)) func)))
    table))

(defun std-newline-out-handler (stream character)
  (declare (ignore character))
  (with-stream-class (simple-stream stream)
    (setf (sm charpos stream) -1)
    nil))

(defun std-tab-out-handler (stream character)
  (declare (ignore character))
  (with-stream-class (simple-stream stream)
    (let ((col (sm charpos stream)))
      (when col
	(setf (sm charpos stream) (1- (* 8 (1+ (floor col 8)))))))
    nil))

(defun std-dc-newline-in-handler (stream character)
  (with-stream-class (dual-channel-simple-stream stream)
    (setf (sm charpos stream) -1) ;; set to 0 "if reading" ???
    character))

(defvar *std-control-out-table*
  (make-control-table #\Newline #'std-newline-out-handler
		      #\Tab     #'std-tab-out-handler))

(defvar *terminal-control-in-table*
  (make-control-table #\Newline #'std-dc-newline-in-handler))

;;;
;;; LOW LEVEL STUFF
;;;

(defun vector-elt-width (vector)
  ;; Return octet-width of vector elements
  (etypecase vector
    ;; missing are: bit, unsigned-byte 2, unsigned-byte 4, signed-byte 30
    ;; [and base-char, which is treated specially]
    ((simple-array (signed-byte 8) (*)) 1)
    ((simple-array (unsigned-byte 8) (*)) 1)
    ((simple-array (signed-byte 16) (*)) 2)
    ((simple-array (unsigned-byte 16) (*)) 2)
    ((simple-array (signed-byte 32) (*)) 4)
    ((simple-array (unsigned-byte 32) (*)) 4)
    ((simple-array single-float (*)) 4)
    ((simple-array double-float (*)) 8)
    ((simple-array (complex single-float) (*)) 8)
    ((simple-array (complex double-float) (*)) 16)))

(defun endian-swap-value (vector endian-swap)
  (case endian-swap
    (:network-order (case (vector-elt-width vector)
		      (1 0)
		      (2 1)
		      (4 3)
		      (8 7)
		      (16 15)))
    (:byte-8 0)
    (:byte-16 1)
    (:byte-32 3)
    (:byte-64 7)
    (:byte-128 15)
    (otherwise endian-swap)))

(defun read-vector (vector stream &key (start 0) end (endian-swap :byte-8))
  (declare (type (sb-kernel:simple-unboxed-array (*)) vector)
	   (type stream stream))
  ;; START and END are octet offsets, not vector indices!  [Except for strings]
  ;; Return value is index of next octet to be read into (i.e., start+count)
  (etypecase stream
    (simple-stream
     (with-stream-class (simple-stream stream)
       (if (stringp vector)
	   (let* ((start (or start 0))
		  (end (or end (length vector)))
		  (char (funcall-stm-handler j-read-char stream nil nil t)))
	     (when char
	       (setf (schar vector start) char)
	       (incf start)
	       (+ start (funcall-stm-handler j-read-chars stream vector nil
					     start end nil))))
	   (do* ((j-read-byte
		  (cond ((any-stream-instance-flags stream :string)
			 (error "Can't READ-BYTE on string streams."))
			((any-stream-instance-flags stream :dual)
			 #'dc-read-byte)
			(t
			 #'sc-read-byte)))
		 (index (or start 0) (1+ index))
		 (end (or end (* (length vector) (vector-elt-width vector))))
		 (endian-swap (endian-swap-value vector endian-swap))
		 (byte (funcall j-read-byte stream nil nil t)
		       (funcall j-read-byte stream nil nil nil)))
		((or (null byte) (>= index end)) index)
	     (setf (bref vector (logxor index endian-swap)) byte)))))
    ((or ansi-stream fundamental-stream)
     (unless (typep vector '(or string
			     (simple-array (signed-byte 8) (*))
			     (simple-array (unsigned-byte 8) (*))))
       (error "Wrong vector type for read-vector on stream not of type simple-stream."))
     (read-sequence vector stream :start (or start 0) :end end))))

#|(defun write-vector ...)|#

;;; TODO: move getpagesize into sbcl/unix.lisp, where it belongs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun getpagesize ()
    (sb-unix::int-syscall ("getpagesize"))))

(defun read-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
	   (type (or null simple-stream-buffer) buffer)
	   (type fixnum start)
	   (type (or null fixnum) end)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (simple-stream stream)
    (let ((fd (sm input-handle stream))
	  (end (or end (sm buf-len stream)))
	  (buffer (or buffer (sm buffer stream))))
      (declare (fixnum end))
      (typecase fd
	(fixnum
	 (let ((flag (sb-sys:wait-until-fd-usable fd :input
                                                  (if blocking nil 0))))
	   (cond
	     ((and (not blocking) (= start end)) (if flag -3 0))
	     ((and (not blocking) (not flag)) 0)
	     (t (block nil
		  (let ((count 0))
		    (declare (type fixnum count))
		    (tagbody
		     again
		       ;; Avoid CMUCL gengc write barrier
		       (do ((i start (+ i ;#.(sb-unix:unix-getpagesize)
                                        (the fixnum (getpagesize)))))
			   ((>= i end))
			 (declare (type fixnum i))
			 (setf (bref buffer i) 0))
		       (setf (bref buffer (1- end)) 0)
		       (multiple-value-bind (bytes errno)
			   (sb-unix:unix-read fd (buffer-sap buffer start)
                                              (the fixnum (- end start)))
			 (declare (type (or null fixnum) bytes)
				  (type (integer 0 100) errno))
			 (when bytes
			   (incf count bytes)
			   (incf start bytes))
			 (cond ((null bytes)
				(format t "~&;; UNIX-READ: errno=~D~%" errno)
				(cond ((= errno sb-unix:eintr) (go again))
				      ((and blocking
					    (or (= errno ;;sb-unix:eagain
                                                   ;; FIXME: move
                                                   ;; eagain into
                                                   ;; sb-unix
                                                   11)
						(= errno sb-unix:ewouldblock)))
				       (sb-sys:wait-until-fd-usable fd :input nil)
				       (go again))
				      (t (return (- -10 errno)))))
			       ((zerop count) (return -1))
			       (t (return count)))))))))))
	(t (error "implement me"))))))

(defun write-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
	   (type (or null simple-stream-buffer) buffer)
	   (type fixnum start)
	   (type (or null fixnum) end))
  (with-stream-class (simple-stream stream)
    (let ((fd (sm output-handle stream))
	  (end (or end (error "WRITE-OCTETS: end=NIL")))
	  (buffer (or buffer (error "WRITE-OCTETS: buffer=NIL"))))
      (typecase fd
	(fixnum
	 (let ((flag (sb-sys:wait-until-fd-usable fd :output
                                                  (if blocking nil 0))))
	   (cond
	     ((and (not blocking) (= start end)) (if flag -3 0))
	     ((and (not blocking) (not flag)) 0)
	     (t
	      (block nil
		(let ((count 0))
		  (tagbody again
		     (multiple-value-bind (bytes errno)
			 (sb-unix:unix-write fd (buffer-sap buffer) start
                                             (- end start))
		       (when bytes
			 (incf count bytes)
			 (incf start bytes))
		       (cond ((null bytes)
			      (format t "~&;; UNIX-WRITE: errno=~D~%" errno)
			      (cond ((= errno sb-unix:eintr) (go again))
				    ;; don't block for subsequent chars
				    (t (return (- -10 errno)))))
			     (t (return count)))))))))))
	(t (error "implement me"))))))


;;;
;;; IMPLEMENTATIONS
;;;

(defmethod device-open ((stream null-simple-stream) options)
  (add-stream-instance-flags stream :simple :input :output)
  stream)

(defmethod device-open ((stream buffer-input-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream buffer-output-simple-stream) options)
  #| do something |#
  stream)

(defun open-file-stream (stream options)
  (let ((filename (getf options :filename))
	(direction (getf options :direction :input))
	(if-exists (getf options :if-exists))
	(if-exists-given (not (getf options :if-exists t)))
	(if-does-not-exist (getf options :if-does-not-exist))
	(if-does-not-exist-given (not (getf options :if-does-not-exist t))))
    (with-stream-class (file-simple-stream stream)
      (ecase direction
	(:input (add-stream-instance-flags stream :input))
	(:output (add-stream-instance-flags stream :output))
	(:io (add-stream-instance-flags stream :input :output)))
      (cond ((and (sm input-handle stream) (sm output-handle stream)
		  (not (eql (sm input-handle stream)
			    (sm output-handle stream))))
	     (error "Input-Handle and Output-Handle can't be different."))
	    ((or (sm input-handle stream) (sm output-handle stream))
	     (add-stream-instance-flags stream :simple)
	     ;; get namestring, etc. from handle, if possible (it's a stream)
	     ;; set up buffers
	     stream)
	    (t
	     (multiple-value-bind (fd namestring original delete-original)
		 (%fd-open filename direction if-exists if-exists-given
			   if-does-not-exist if-does-not-exist-given)
	       (when fd
		 (add-stream-instance-flags stream :simple)
		 (setf (sm pathname stream) filename
		       (sm filename stream) namestring
		       (sm original stream) original
		       (sm delete-original stream) delete-original)
		 (when (any-stream-instance-flags stream :input)
		   (setf (sm input-handle stream) fd))
		 (when (any-stream-instance-flags stream :output)
		   (setf (sm output-handle stream) fd))
		 (sb-ext:finalize stream
		   (lambda ()
		     (sb-unix:unix-close fd)
		     (format *terminal-io* "~&;;; ** closed ~S (fd ~D)~%"
			     namestring fd)))
		 stream)))))))

(defmethod device-open ((stream file-simple-stream) options)
  (with-stream-class (file-simple-stream stream)
    (when (open-file-stream stream options)
      ;; Franz says:
      ;;  "The device-open method must be prepared to recognize resource
      ;;   and change-class situations. If no filename is specified in
      ;;   the options list, and if no input-handle or output-handle is
      ;;   given, then the input-handle and output-handle slots should
      ;;   be examined; if non-nil, that means the stream is still open,
      ;;   and thus the operation being requested of device-open is a
      ;;   change-class. Also, a device-open method need not allocate a
      ;;   buffer every time it is called, but may instead reuse a
      ;;   buffer it finds in a stream, if it does not become a security
      ;;   issue."
      (unless (sm buffer stream)
	(let ((length (device-buffer-length stream)))
	  ;; Buffer should be array of (unsigned-byte 8), in general
	  ;; use strings for now so it's easy to read the content...
	  (setf (sm buffer stream) (make-string length)
		(sm buffpos stream) 0
		(sm buffer-ptr stream) 0
		(sm buf-len stream) length)))
      (when (any-stream-instance-flags stream :output)
	(setf (sm control-out stream) *std-control-out-table*))
      (install-single-channel-character-strategy
       stream (getf options :external-format :default) nil))))

(defmethod device-open ((stream mapped-file-simple-stream) options)
  (with-stream-class (mapped-file-simple-stream stream)
    (when (open-file-stream stream options)
      (let* ((input (any-stream-instance-flags stream :input))
	     (output (any-stream-instance-flags stream :output))
	     (prot (logior (if input PROT-READ 0)
			   (if output PROT-WRITE 0)))
	     (fd (or (sm input-handle stream) (sm output-handle stream))))
	(multiple-value-bind (okay dev ino mode nlink uid gid rdev size)
	    (sb-unix:unix-fstat fd)
	  (declare (ignore ino mode nlink uid gid rdev))
	  (unless okay
	    (sb-unix:unix-close fd)
	    (sb-ext:cancel-finalization stream)
	    (error "Error fstating ~S: ~A" stream
		   (sb-int:strerror dev)))
	  (when (> size most-positive-fixnum)
	    ;; Or else BUF-LEN has to be a general integer, or
	    ;; maybe (unsigned-byte 32).  In any case, this means
	    ;; BUF-MAX and BUF-PTR have to be the same, which means
	    ;; number-consing every time BUF-PTR moves...
	    ;; Probably don't have the address space available to map
	    ;; bigger files, anyway.  Maybe DEVICE-EXTEND can adjust
	    ;; the mapped portion of the file?
	    (warn "Unable to memory-map entire file.")
	    (setf size most-positive-fixnum))
	  (let ((buffer
		 (sb-unix:unix-mmap nil size prot MAP-SHARED fd 0)))
	    (when (null buffer)
	      (sb-unix:unix-close fd)
	      (sb-ext:cancel-finalization stream)
	      (error "Unable to map file."))
	    (setf (sm buffer stream) buffer
		  (sm buffpos stream) 0
		  (sm buffer-ptr stream) size
		  (sm buf-len stream) size)
	    (install-single-channel-character-strategy
	     stream (getf options :external-format :default) 'mapped)
	    (sb-ext:finalize stream
	      (lambda ()
		(sb-unix:unix-munmap buffer size)
		(format *terminal-io* "~&;;; ** unmapped ~S" buffer)))))))
    stream))

(defmethod device-open ((stream string-input-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream string-output-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream xp-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream fill-pointer-output-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream socket-base-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream socket-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-open ((stream terminal-simple-stream) options)
  (with-stream-class (terminal-simple-stream stream)
    (when (getf options :input-handle)
      (setf (sm input-handle stream) (getf options :input-handle))
      (add-stream-instance-flags stream :simple :interactive :dual :input)
      (unless (sm buffer stream)
	(let ((length (device-buffer-length stream)))
	  (setf (sm buffer stream) (make-string length)
		(sm buf-len stream) length)))
      (setf (sm control-in stream) *terminal-control-in-table*))
    (when (getf options :output-handle)
      (setf (sm output-handle stream) (getf options :output-handle))
      (add-stream-instance-flags stream :simple :interactive :dual :output)
      (unless (sm out-buffer stream)
	(let ((length (device-buffer-length stream)))
	  (setf (sm out-buffer stream) (make-string length)
		(sm max-out-pos stream) length)))
      (setf (sm control-out stream) *std-control-out-table*))
    (install-dual-channel-character-strategy
     stream (getf options :external-format :default)))
  #| do something |#
  stream)


(defmethod device-close :around ((stream simple-stream) abort)
  (with-stream-class (simple-stream stream)
    (when (any-stream-instance-flags stream :input :output)
      (when (any-stream-instance-flags stream :output)
	(if abort
	    (clear-output stream)
	    (force-output stream)))
      (call-next-method)
      (setf (sm input-handle stream) nil
	    (sm output-handle stream) nil)
      (remove-stream-instance-flags stream :input :output)
      (sb-ext:cancel-finalization stream))))

(defmethod device-close ((stream simple-stream) abort)
  (declare (ignore abort))
  t)

(defmethod device-close ((stream file-simple-stream) abort)
  (with-stream-class (file-simple-stream stream)
    (cond (abort
	   ;; Remove any fd-handler
	   ;; If it's an output stream and has an original name,
	   ;; revert the file
	   )
	  (t
	   ;; If there's an original name and delete-original is set
	   ;; kill the original
	   ))
    (if (sm input-handle stream)
	(sb-unix:unix-close (sm input-handle stream))
	(sb-unix:unix-close (sm output-handle stream)))
    (setf (sm buffer stream) nil))
  t)

(defmethod device-close ((stream mapped-file-simple-stream) abort)
  (with-stream-class (mapped-file-simple-stream stream)
    (when (sm buffer stream)
      (sb-unix:unix-munmap (sm buffer stream) (sm buf-len stream))
      (setf (sm buffer stream) nil))
    (cond (abort
	   ;; remove any FD handler
	   ;; if it has an original name (is this possible for mapped files?)
	   ;;   revert the file
	   )
	  (t
	   ;; if there's an original name and delete-original is set (again,
	   ;;   is this even possible?), kill the original
	   ))
    (sb-unix:unix-close (sm input-handle stream)))
  t)


(defmethod device-buffer-length ((stream simple-stream))
  4096)

(defmethod device-buffer-length ((stream null-simple-stream))
  256)


(defmethod device-file-position ((stream simple-stream))
  (with-stream-class (simple-stream stream)
    ;; this may be wrong if :DUAL flag is set!
    (sm buffpos stream)))

(defmethod (setf device-file-position) (value (stream simple-stream))
  (with-stream-class (simple-stream stream)
    ;; this may be wrong if :DUAL flag is set!
    (setf (sm buffpos stream) value)))

(defmethod device-file-position ((stream string-simple-stream))
  ;; get string length (of input or output buffer?)
  )

(defmethod (setf device-file-position) (value (stream string-simple-stream))
  ;; set string length (of input or output buffer?)
  )

(defmethod device-file-position ((stream fill-pointer-output-simple-stream))
  ;; get fill pointer (of input or output buffer?)
  )

(defmethod (setf device-file-position)
    (value (stream fill-pointer-output-simple-stream))
  ;; set fill pointer (of input or output buffer?)
  )

(defmethod device-file-position ((stream file-simple-stream))
  (with-stream-class (file-simple-stream stream)
    (values (sb-unix:unix-lseek (or (sm input-handle stream)
                                    (sm output-handle stream))
                                0
                                sb-unix:l_incr))))

(defmethod (setf device-file-position) (value (stream file-simple-stream))
  (declare (type fixnum value))
  (with-stream-class (file-simple-stream stream)
    (values (sb-unix:unix-lseek (or (sm input-handle stream)
                                    (sm output-handle stream))
                                value
                                (if (minusp value)
                                    sb-unix:l_xtnd
                                    sb-unix:l_set)))))


(defmethod device-file-length ((stream simple-stream))
  nil)

(defmethod device-file-length ((stream direct-simple-stream))
  ;; return buffer length
  )

(defmethod device-file-length ((stream string-simple-stream))
  ;; return string length
  )

(defmethod device-file-length ((stream file-simple-stream))
  (with-stream-class (file-simple-stream stream)
    (multiple-value-bind (okay dev ino mode nlink uid gid rdev size)
	(sb-unix:unix-fstat (sm input-handle stream))
      (declare (ignore dev ino mode nlink uid gid rdev))
      (if okay size nil))))


(defmethod device-read ((stream single-channel-simple-stream) buffer
			start end blocking)
;;  (when (and (null buffer) (not (eql start end)))
;;    (with-stream-class (single-channel-simple-stream stream)
;;      (setq buffer (sm buffer stream))
;;      (setq end (sm buf-len stream))))
  (read-octets stream buffer start end blocking))

(defmethod device-read ((stream dual-channel-simple-stream) buffer
			start end blocking)
  (when (null buffer)
    (with-stream-class (dual-channel-simple-stream stream)
      (setq buffer (sm buffer stream))
      (setq end (- (sm buf-len stream) start))))
  (read-octets stream buffer start end blocking))

(defmethod device-read ((stream null-simple-stream) buffer
			start end blocking)
  (declare (ignore buffer start end blocking))
  -1)

(defmethod device-read ((stream terminal-simple-stream) buffer
			start end blocking)
  (let ((result (call-next-method)))
    (if (= result -1) -2 result)))


(defmethod device-clear-input ((stream simple-stream) buffer-only)
  (declare (ignore buffer-only))
  nil)

(defmethod device-clear-input ((stream terminal-simple-stream) buffer-only)
  )


(defmethod device-write ((stream single-channel-simple-stream) buffer
			 start end blocking)
  (when (and (null buffer) (not (eql start end)))
    (with-stream-class (single-channel-simple-stream stream)
      (setf buffer (sm buffer stream))
      (setf end (sm buffpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-write ((stream dual-channel-simple-stream) buffer
			 start end blocking)
  (when (and (null buffer) (not (eql start end)))
    (with-stream-class (dual-channel-simple-stream stream)
      (setf buffer (sm out-buffer stream))
      (setf end (sm outpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-write ((stream null-simple-stream) buffer
			 start end blocking)
  (declare (ignore buffer blocking))
  (- end start))

(defmethod device-write ((stream socket-base-simple-stream) buffer
			 start end blocking)
  ;; @@2
  (call-next-method))


(defmethod device-clear-output ((stream simple-stream))
  nil)


(defmethod device-extend ((stream direct-simple-stream) need action)
  (declare (ignore need action))
  nil)

(defmethod device-extend ((stream string-input-simple-stream) need action)
  (declare (ignore need action))
  nil)

(defmethod device-extend ((stream string-output-simple-stream) need action)
  ;; @@3
  )

(defmethod device-extend ((stream fill-pointer-output-simple-stream)
			  need action)
  ;; @@4
  )

(defmethod device-extend ((stream mapped-file-simple-stream) need action)
  (declare (ignore need action))
  nil)


;; device-finish-record apparently has no methods defined


;;;
;;; IMPLEMENTATIONS FOR FOREIGN STREAMS
;;; (SYS:LISP-STREAM AND EXT:FUNDAMENTAL-STREAM)
;;;


;;;
;;; CREATION OF STANDARD STREAMS
;;;

