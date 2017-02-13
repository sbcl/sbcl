;;; -*- lisp -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;

;;; Sbcl port by Rudi Schlatte.

(in-package "SB-SIMPLE-STREAMS")

;;;
;;; **********************************************************************
;;;
;;; Definition of File-Simple-Stream and relations

(def-stream-class file-simple-stream (single-channel-simple-stream file-stream)
  ((pathname :initform nil :initarg :pathname)
   (filename :initform nil :initarg :filename)
   (original :initform nil :initarg :original)
   (delete-original :initform nil :initarg :delete-original)))

(def-stream-class mapped-file-simple-stream (file-simple-stream
                                             direct-simple-stream)
  ())

(def-stream-class probe-simple-stream (simple-stream)
  ((pathname :initform nil :initarg :pathname)))

(defmethod print-object ((object file-simple-stream) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (with-stream-class (file-simple-stream object)
      (cond ((not (any-stream-instance-flags object :simple))
             (princ "Invalid " stream))
            ((not (any-stream-instance-flags object :input :output))
             (princ "Closed " stream)))
      (format stream "~:(~A~) for ~S"
              (type-of object) (sm filename object)))))


(defun open-file-stream (stream options)
  (let ((filename (pathname (getf options :filename)))
        (direction (getf options :direction :input))
        (if-exists (getf options :if-exists))
        (if-exists-given (not (eql (getf options :if-exists t) t)))
        (if-does-not-exist (getf options :if-does-not-exist))
        (if-does-not-exist-given (not (eql (getf options :if-does-not-exist t) t))))
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
             ;; get namestring, etc., from handle, if possible
             ;;    (i.e., if it's a stream)
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
                             namestring fd)
                     (when original
                       (revert-file namestring original)))
                   :dont-save t)
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
          (setf (sm buffer stream) (allocate-buffer length)
                (sm buffpos stream) 0
                (sm buffer-ptr stream) 0
                (sm buf-len stream) length)))
      (when (any-stream-instance-flags stream :output)
        (setf (sm control-out stream) *std-control-out-table*))
      (setf (stream-external-format stream)
            (getf options :external-format :default))
      stream)))

;;;   Revert a file, if possible; otherwise just delete it.  Used during
;;; CLOSE when the abort flag is set.
;;;
;;; TODO: use this in src/code/fd-stream.lisp:fd-stream-misc-routine
;;; as well, snarf error reporting from there.
(defun revert-file (filename original)
  (declare (type simple-string filename)
           (type (or simple-string null) original))
  ;; We can't do anything unless we know what file were
  ;; dealing with, and we don't want to do anything
  ;; strange unless we were writing to the file.
  (if original
      (multiple-value-bind (okay err) (sb-unix:unix-rename original filename)
        (unless okay
          (cerror "Go on as if nothing bad happened."
                  "Could not restore ~S to its original contents: ~A"
                  filename (sb-int:strerror err))))
      ;; We can't restore the original, so nuke that puppy.
      (multiple-value-bind (okay err) (sb-unix:unix-unlink filename)
        (unless okay
          (cerror "Go on as if nothing bad happened."
                  "Could not remove ~S: ~A"
                  filename (sb-int:strerror err))))))

;;; DELETE-ORIGINAL -- internal
;;;
;;;   Delete a backup file.  Used during CLOSE.
;;;
;;; TODO: use this in src/code/fd-stream.lisp:fd-stream-misc-routine
;;; as well, snarf error reporting from there.
(defun delete-original (filename original)
  (declare (type simple-string filename)
           (type (or simple-string null) original))
  (when original
    (multiple-value-bind (okay err) (sb-unix:unix-unlink original)
      (unless okay
        (cerror "Go on as if nothing bad happened."
                "Could not delete ~S during close of ~S: ~A"
                original filename (sb-int:strerror err))))))

(defmethod device-close ((stream file-simple-stream) abort)
  (with-stream-class (file-simple-stream stream)
    (let ((fd (or (sm input-handle stream) (sm output-handle stream)))
          (closed nil))
      (when (integerp fd)
        (cond (abort
               (when (any-stream-instance-flags stream :output)
                 #+win32 (progn (sb-unix:unix-close fd) (setf closed t))
                 (revert-file (sm filename stream) (sm original stream))))
              (t
               (when (sm delete-original stream)
                 (delete-original (sm filename stream) (sm original stream)))))
        (unless closed
          (sb-unix:unix-close fd)))
      (when (sm buffer stream)
        (free-buffer (sm buffer stream))
        (setf (sm buffer stream) nil))))
  t)

(defmethod device-file-position ((stream file-simple-stream))
  (with-stream-class (file-simple-stream stream)
    (let ((fd (or (sm input-handle stream) (sm output-handle stream))))
      (if (integerp fd)
          (values (sb-unix:unix-lseek fd 0 sb-unix:l_incr))
          (file-position fd)))))

(defmethod (setf device-file-position) (value (stream file-simple-stream))
  (declare (type fixnum value))
  (with-stream-class (file-simple-stream stream)
    (let ((fd (or (sm input-handle stream) (sm output-handle stream))))
      (if (integerp fd)
          (values (sb-unix:unix-lseek fd
                                      (if (minusp value) (1+ value) value)
                                      (if (minusp value) sb-unix:l_xtnd sb-unix:l_set)))
          (file-position fd value)))))

(defmethod device-file-length ((stream file-simple-stream))
  (with-stream-class (file-simple-stream stream)
    (let ((fd (or (sm input-handle stream) (sm output-handle stream))))
      (if (integerp fd)
          (multiple-value-bind (okay dev ino mode nlink uid gid rdev size)
              (sb-unix:unix-fstat (sm input-handle stream))
            (declare (ignore dev ino mode nlink uid gid rdev))
            (if okay size nil))
          (file-length fd)))))

(defmethod device-open ((stream mapped-file-simple-stream) options)
  (with-stream-class (mapped-file-simple-stream stream)
    (when (open-file-stream stream options)
      (let* ((input (any-stream-instance-flags stream :input))
             (output (any-stream-instance-flags stream :output))
             (prot (logior (if input sb-posix::PROT-READ 0)
                           (if output sb-posix::PROT-WRITE 0)))
             (fd (or (sm input-handle stream) (sm output-handle stream))))
        (unless (integerp fd)
          (error "Can't memory-map an encapsulated stream."))
        (multiple-value-bind (okay dev ino mode nlink uid gid rdev size)
            (sb-unix:unix-fstat fd)
          (declare (ignore ino mode nlink uid gid rdev))
          (unless okay
            (sb-unix:unix-close fd)
            (sb-ext:cancel-finalization stream)
            (error "Error fstating ~S: ~A" stream
                   (sb-int:strerror dev)))
          (when (>= size most-positive-fixnum)
            ;; Or else BUF-LEN has to be a general integer, or
            ;; maybe (unsigned-byte 32).  In any case, this means
            ;; BUF-MAX and BUF-PTR have to be the same, which means
            ;; number-consing every time BUF-PTR moves...
            ;; Probably don't have the address space available to map
            ;; bigger files, anyway.  Maybe DEVICE-READ can adjust
            ;; the mapped portion of the file when necessary?
            (warn "Unable to memory-map entire file.")
            (setf size (1- most-positive-fixnum)))
          (let ((buffer
                 #-win32
                 (handler-case
                     (sb-posix:mmap nil size prot sb-posix::MAP-SHARED fd 0)
                   (sb-posix:syscall-error nil))
                 #+win32
                 (let ((mapping
                         (sb-win32:create-file-mapping fd nil 2 0 size nil)))
                   (typecase mapping
                     ((integer -1 0) nil)
                     (t (let ((sap (prog1 (sb-win32:map-view-of-file
                                           mapping 4 0 0 size)
                                     (sb-win32:close-handle mapping))))
                          (and (not (zerop (sb-sys:sap-int sap))) sap)))))))
            (when (null buffer)
              (sb-unix:unix-close fd)
              (sb-ext:cancel-finalization stream)
              (error "Unable to map file."))
            (setf (sm buffer stream) buffer
                  (sm buffpos stream) 0
                  (sm buffer-ptr stream) size
                  (sm buf-len stream) size)
            (when (any-stream-instance-flags stream :output)
              (setf (sm control-out stream) *std-control-out-table*))
            (let ((efmt (getf options :external-format :default)))
              (compose-encapsulating-streams stream efmt)
              (setf (stream-external-format stream) efmt)
              ;; overwrite the strategy installed in :after method of
              ;; (setf stream-external-format)
              (install-single-channel-character-strategy
               (melding-stream stream) efmt 'mapped))
            (sb-ext:finalize stream
              (lambda ()
                #+win32 (sb-win32:unmap-view-of-file buffer)
                #-win32 (sb-posix:munmap buffer size)
                (format *terminal-io* "~&;;; ** unmapped ~S" buffer))
              :dont-save t))))
      stream)))


(defmethod device-close ((stream mapped-file-simple-stream) abort)
  (with-stream-class (mapped-file-simple-stream stream)
    (when (sm buffer stream)
      #+win32 (sb-win32:unmap-view-of-file (sm buffer stream))
      #-win32 (sb-posix:munmap (sm buffer stream) (sm buf-len stream))
      (setf (sm buffer stream) nil))
    (sb-unix:unix-close (or (sm input-handle stream) (sm output-handle stream))))
  t)

(defmethod device-write ((stream mapped-file-simple-stream) buffer
                         start end blocking)
  (assert (eq buffer :flush) (buffer)) ; finish/force-output
  (with-stream-class (mapped-file-simple-stream stream)
    (sb-posix:msync (sm buffer stream) (sm buf-len stream)
                    (if blocking sb-posix::ms-sync sb-posix::ms-async))))

(defmethod device-open ((stream probe-simple-stream) options)
  (let ((pathname (getf options :filename)))
    (with-stream-class (probe-simple-stream stream)
      (add-stream-instance-flags stream :simple)
      (when (sb-unix:unix-access (%file-namestring pathname) sb-unix:f_ok)
        (setf (sm pathname stream) pathname)
        t))))
