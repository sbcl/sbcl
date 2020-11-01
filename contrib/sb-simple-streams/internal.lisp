;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-

;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;

;;; Sbcl port by Rudi Schlatte.

(in-package "SB-SIMPLE-STREAMS")

;;;
;;; **********************************************************************
;;;
;;; Various functions needed by simple-streams
(declaim (inline buffer-sap bref (setf bref) buffer-copy
                 allocate-buffer free-buffer))

(defun buffer-sap (thing &optional offset)
  (declare (type simple-stream-buffer thing) (type (or fixnum null) offset)
           (optimize (speed 3) (space 2) (debug 0) (safety 0)
                     ;; Suppress the note about having to box up the return:
                     (sb-ext:inhibit-warnings 3)))
  (let ((sap (if (vectorp thing) (sb-sys:vector-sap thing) thing)))
    (if offset (sb-sys:sap+ sap offset) sap)))

(defun bref (buffer index)
  (declare (type simple-stream-buffer buffer)
           (type (integer 0 #.most-positive-fixnum) index))
  (if (vectorp buffer)
      (sb-sys:with-pinned-objects (buffer)
        (sb-sys:sap-ref-8 (sb-sys:vector-sap buffer) index))
      (sb-sys:sap-ref-8 buffer index)))

(defun (setf bref) (octet buffer index)
  (declare (type (unsigned-byte 8) octet)
           (type simple-stream-buffer buffer)
           (type (integer 0 #.most-positive-fixnum) index))
  (if (vectorp buffer)
      (sb-sys:with-pinned-objects (buffer)
        (setf (sb-sys:sap-ref-8 (sb-sys:vector-sap buffer) index) octet))
      (setf (sb-sys:sap-ref-8 buffer index) octet)))

(defun buffer-copy (src soff dst doff length)
  (declare (type simple-stream-buffer src dst)
           (type fixnum soff doff length))
  (sb-sys:with-pinned-objects (src dst)
   (sb-kernel:system-area-ub8-copy (buffer-sap src) soff
                                   (buffer-sap dst) doff
                                   length)))

(defun allocate-buffer (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defun free-buffer (buffer)
  (sb-int:aver (typep buffer '(simple-array (unsigned-byte 8) (*))))
  t)

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
    ;; FIXME: Currently, -1 is wrong, since callers of CHARPOS expect
    ;; a result in (or null (and fixnum unsigned-byte)), so they must
    ;; never see this temporary value.  Note that in
    ;; STD-NEWLINE-OUT-HANDLER it is correct to use -1, since CHARPOS
    ;; is incremented to zero before WRITE-CHAR returns.  Perhaps the
    ;; same should happen for input?
    (setf (sm charpos stream) 0) ; was -1
    character))

(defvar *std-control-out-table*
  (make-control-table #\Newline #'std-newline-out-handler
                      #\Tab     #'std-tab-out-handler))

(defvar *default-external-format* :iso8859-1)

(defvar *external-formats* (make-hash-table))
(defvar *external-format-aliases* (make-hash-table))

(defstruct (external-format
             (:conc-name ef-)
             (:print-function %print-external-format)
             (:constructor make-external-format (name octets-to-char
                                                      char-to-octets)))
  (name (sb-int:missing-arg) :type keyword :read-only t)
  (octets-to-char (sb-int:missing-arg) :type function :read-only t)
  (char-to-octets (sb-int:missing-arg) :type function :read-only t))

(defun %print-external-format (ef stream depth)
  (declare (ignore depth))
  (print-unreadable-object (ef stream :type t :identity t)
    (princ (ef-name ef) stream)))

(defmacro define-external-format (name octets-to-char char-to-octets)
  `(macrolet ((octets-to-char ((state input unput) &body body)
                `(lambda (,state ,input ,unput)
                   (declare (type (function () (unsigned-byte 8)) ,input)
                            (type (function (sb-int:index) t) ,unput)
                            (ignorable ,state ,input ,unput)
                            (values character sb-int:index t))
                   ,@body))
              (char-to-octets ((char state output) &body body)
                `(lambda (,char ,state ,output)
                   (declare (type character ,char)
                            (type (function ((unsigned-byte 8)) t) ,output)
                            (ignorable state ,output)
                            (values t))
                   ,@body)))
     (setf (gethash ,name *external-formats*)
           (make-external-format ,name ,octets-to-char ,char-to-octets))))

;;; TODO: make this work
(defun load-external-format-aliases ()
  (let ((*package* (find-package "KEYWORD")))
    (with-open-file (stm "ef:aliases" :if-does-not-exist nil)
      (when stm
        (do ((alias (read stm nil stm) (read stm nil stm))
             (value (read stm nil stm) (read stm nil stm)))
            ((or (eq alias stm) (eq value stm))
             (unless (eq alias stm)
               (warn "External-format aliases file ends early.")))
          (if (and (keywordp alias) (keywordp value))
              (setf (gethash alias *external-format-aliases*) value)
              (warn "Bad entry in external-format aliases file: ~S => ~S."
                    alias value)))))))

(defun find-external-format (name &optional (error-p t))
  (when (external-format-p name)
    (return-from find-external-format name))

  (when (eq name :default)
    (setq name *default-external-format*))

  ;; TODO: make this work
  #+nil
  (unless (ext:search-list-defined-p "ef:")
    (setf (ext:search-list "ef:") '("library:ef/")))

  (when (zerop (hash-table-count *external-format-aliases*))
    (setf (gethash :latin1 *external-format-aliases*) :iso8859-1)
    (setf (gethash :latin-1 *external-format-aliases*) :iso8859-1)
    (setf (gethash :iso-8859-1 *external-format-aliases*) :iso8859-1)
    (load-external-format-aliases))

  (do ((tmp (gethash name *external-format-aliases*)
            (gethash tmp *external-format-aliases*))
       (cnt 0 (1+ cnt)))
      ((or (null tmp) (= cnt 50))
       (unless (null tmp)
         (error "External-format aliasing depth exceeded.")))
    (setq name tmp))

  (or (gethash name *external-formats*)
      (and (let ((*package* (find-package "SB-SIMPLE-STREAMS")))
             (load (format nil "ef:~(~A~)" name) :if-does-not-exist nil))
           (gethash name *external-formats*))
      (if error-p (error "External format ~S not found." name) nil)))

(define-condition void-external-format (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream "Attempting I/O through void external-format."))))

(define-external-format :void
    (octets-to-char (state input unput)
      (declare (ignore state input unput))
      (error 'void-external-format))
  (char-to-octets (char state output)
    (declare (ignore char state output))
    (error 'void-external-format)))

(define-external-format :iso8859-1
    (octets-to-char (state input unput)
      (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
      (values (code-char (funcall input)) 1 state))
  (char-to-octets (char state output)
    (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
    (let ((code (char-code char)))
      #-(or)
      (funcall output code)
      #+(or)
      (if (< code 256)
          (funcall output code)
          (funcall output (char-code #\?))))
    state))

(defmacro octets-to-char (external-format state count input unput)
  (let ((tmp1 (gensym)) (tmp2 (gensym)) (tmp3 (gensym)))
    `(multiple-value-bind (,tmp1 ,tmp2 ,tmp3)
         (funcall (ef-octets-to-char ,external-format) ,state ,input ,unput)
       (setf ,state ,tmp3 ,count ,tmp2)
       ,tmp1)))

(defmacro char-to-octets (external-format char state output)
  `(progn
     (setf ,state (funcall (ef-char-to-octets ,external-format)
                           ,char ,state ,output))
     nil))

(defun string-to-octets (string &key (start 0) end (external-format :default))
  (declare (type string string)
           (type sb-int:index start)
           (type (or null sb-int:index) end))
  (let ((ef (find-external-format external-format))
        (buffer (make-array (length string) :element-type '(unsigned-byte 8)))
        (ptr 0)
        (state nil))
    (flet ((out (b)
             (setf (aref buffer ptr) b)
             (when (= (incf ptr) (length buffer))
               (setq buffer (adjust-array buffer (* 2 ptr))))))
      (dotimes (i (- (or end (length string)) start))
        (declare (type sb-int:index i))
        (char-to-octets ef (char string (+ start i)) state #'out))
      (sb-kernel:shrink-vector buffer ptr))))

(defun octets-to-string (octets &key (start 0) end (external-format :default))
  (declare (type vector octets)
           (type sb-int:index start)
           (type (or null sb-int:index) end))
  (let ((ef (find-external-format external-format))
        (end (1- (or end (length octets))))
        (string (make-string (length octets)))
        (ptr (1- start))
        (pos -1)
        (count 0)
        (state nil))
    (declare (ignorable count))
    (flet ((input ()
             (aref octets (incf ptr)))
           (unput (n)
             (decf ptr n)))
      (loop until (>= ptr end)
            do (setf (schar string (incf pos))
                 (octets-to-char ef state count #'input #'unput))))
    (sb-kernel:shrink-vector string (1+ pos))))

(defun vector-elt-width (vector)
  ;; Return octet-width of vector elements
  (etypecase vector
    ;; (simple-array fixnum (*)) not supported
    ;; (simple-array base-char (*)) treated specially; don't call this
    ((simple-array bit (*)) 1)
    ((simple-array (unsigned-byte 2) (*)) 1)
    ((simple-array (unsigned-byte 4) (*)) 1)
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

#-(or big-endian little-endian)
(eval-when (:compile-toplevel)
  (push sb-c::*backend-byte-order* *features*))

(defun endian-swap-value (vector endian-swap)
  #+big-endian (declare (ignore vector))
  (case endian-swap
    (:network-order #+big-endian 0
                    #+little-endian (1- (vector-elt-width vector)))
    (:byte-8 0)
    (:byte-16 1)
    (:byte-32 3)
    (:byte-64 7)
    (:byte-128 15)
    (otherwise endian-swap)))

#+(or)
(defun %read-vector (vector stream start end endian-swap blocking)
  (declare (type (kernel:simple-unboxed-array (*)) vector)
           (type stream stream))
  ;; move code from read-vector
  )

#+(or)
(defun %write-vector (... blocking)
  ;; implement me
  )

(defun read-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
           (type (or null simple-stream-buffer) buffer)
           (type fixnum start)
           (type (or null fixnum) end)
           (type blocking blocking)
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
                       (do ((i start (+ i #.(sb-posix:getpagesize))))
                           ((>= i end))
                         (declare (type fixnum i))
                         (setf (bref buffer i) 0))
                       (setf (bref buffer (1- end)) 0)
                       (multiple-value-bind (bytes errno)
                           (sb-sys:with-pinned-objects (buffer)
                             (sb-unix:unix-read fd (buffer-sap buffer start)
                                                (the fixnum (- end start))))
                         (declare (type (or null fixnum) bytes)
                                  (type (integer 0 100) errno))
                         (when bytes
                           (incf count bytes)
                           (incf start bytes))
                         (cond ((null bytes)
                                (format *debug-io* "~&;; UNIX-READ: errno=~D~%" errno)
                                (cond ((= errno sb-unix:eintr) (go again))
                                      ((and blocking
                                            (or (= errno ;;sb-unix:eagain
                                                   ;; TODO: move
                                                   ;; eagain into
                                                   ;; sb-unix
                                                   11)
                                                (= errno
                                                   #-win32
                                                   sb-unix:ewouldblock
                                                   #+win32
                                                   sb-unix:eintr)))
                                       (sb-sys:wait-until-fd-usable fd :input nil)
                                       (go again))
                                      (t (return (- -10 errno)))))
                               ((zerop count) (return -1))
                               (t (return count)))))))))))
        (t ;; (%read-vector buffer fd start end :byte-8
           ;;               (if blocking :bnb nil))
         )))))

(defun write-octets (stream buffer start end blocking)
  (declare (type simple-stream stream)
           (type simple-stream-buffer buffer)
           (type fixnum start)
           (type (or null fixnum) end))
  (with-stream-class (simple-stream stream)
    (when (sm handler stream)
      (do ()
          ((null (sm pending stream)))
        (sb-sys:serve-all-events)))

    (let ((fd (sm output-handle stream))
          (end (or end (length buffer))))
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
                         (sb-sys:with-pinned-objects (buffer)
                           (sb-unix:unix-write fd (buffer-sap buffer) start
                                               (- end start)))
                       (when bytes
                         (incf count bytes)
                         (incf start bytes))
                       (cond ((null bytes)
                              (format *debug-io* "~&;; UNIX-WRITE: errno=~D~%" errno)
                              (cond ((= errno sb-unix:eintr) (go again))
                                    ;; don't block for subsequent chars
                                    (t (return (- -10 errno)))))
                             (t (return count)))))))))))
        (t (error "implement me"))))))

(defun do-some-output (stream)
  ;; Do some pending output; return T if completed, NIL if more to do
  (with-stream-class (simple-stream stream)
    (let ((fd (sm output-handle stream)))
      (loop
        (let ((list (pop (sm pending stream))))
          (unless list
            (sb-sys:remove-fd-handler (sm handler stream))
            (setf (sm handler stream) nil)
            (return t))
          (let* ((buffer (first list))
                 (start (second list))
                 (end (third list))
                 (len (- end start)))
            (declare (type simple-stream-buffer buffer)
                     (type sb-int:index start end len))
            (tagbody again
               (multiple-value-bind (bytes errno)
                   (sb-sys:with-pinned-objects (buffer)
                     (sb-unix:unix-write fd (buffer-sap buffer) start len))
                 (cond ((null bytes)
                        (if (= errno sb-unix:eintr)
                            (go again)
                            (progn (push list (sm pending stream))
                                   (return nil))))
                       ((< bytes len)
                        (setf (second list) (+ start bytes))
                        (push list (sm pending stream))
                        (return nil))
                       ((= bytes len)
                        (free-buffer buffer)))))))))))

(defun queue-write (stream buffer start end)
  ;; Queue a write; return T if buffer needs changing, NIL otherwise
  (declare (type simple-stream stream)
           (type simple-stream-buffer buffer)
           (type sb-int:index start end))
  (with-stream-class (simple-stream stream)
    (when (sm handler stream)
      (unless (do-some-output stream)
        (let ((last (last (sm pending stream))))
          (setf (cdr last) (list (list buffer start end)))
          (return-from queue-write t))))
    (let ((bytes (write-octets stream buffer start end nil)))
      (unless (or (= bytes (- end start)) ; completed
                  (= bytes -3)) ; empty buffer; shouldn't happen
        (setf (sm pending stream) (list (list buffer start end)))
        (setf (sm handler stream)
              (sb-sys:add-fd-handler (sm output-handle stream) :output
                                     (lambda (fd)
                                       (declare (ignore fd))
                                       (do-some-output stream))))
        t))))




(defun %fd-open (pathname direction if-exists if-exists-given
                          if-does-not-exist if-does-not-exist-given)
  (declare (type pathname pathname)
           (type (member :input :output :io :probe) direction)
           (type (member :error :new-version :rename :rename-and-delete
                         :overwrite :append :supersede nil) if-exists)
           (type (member :error :create nil) if-does-not-exist))
  (multiple-value-bind (input output mask)
      (ecase direction
        (:input (values t nil sb-unix:o_rdonly))
        (:output (values nil t sb-unix:o_wronly))
        (:io (values t t sb-unix:o_rdwr))
        (:probe (values t nil sb-unix:o_rdonly)))
    (declare (type sb-int:index mask))
    (let* ((phys (sb-int:physicalize-pathname (merge-pathnames pathname)))
           (true (probe-file phys))
           (name (cond (true
                        (sb-ext:native-namestring true :as-file t))
                       ((or (not input)
                            (and input (eq if-does-not-exist :create))
                            (and (eq direction :io) (not if-does-not-exist-given)))
                        (sb-ext:native-namestring phys :as-file t)))))
      ;; Process if-exists argument if we are doing any output.
      (cond (output
             (unless if-exists-given
               (setf if-exists
                     (if (eq (pathname-version pathname) :newest)
                         :new-version
                         :error)))
             (case if-exists
               ((:error nil :new-version)
                (setf mask (logior mask sb-unix:o_excl)))
               ((:rename :rename-and-delete)
                (setf mask (logior mask sb-unix:o_creat)))
               ((:supersede)
                (setf mask (logior mask sb-unix:o_trunc)))))
            (t
             (setf if-exists nil)))     ; :ignore-this-arg
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
      (if (eq if-does-not-exist :create)
          (setf mask (logior mask sb-unix:o_creat)))

      (let ((original (if (member if-exists
                                  '(:rename :rename-and-delete))
                          (sb-impl::pick-backup-name name)
                          nil))
            (delete-original (eq if-exists :rename-and-delete))
            (mode #o666))
        (when original
          ;; We are doing a :rename or :rename-and-delete.
          ;; Determine if the file already exists, make sure the original
          ;; file is not a directory and keep the mode
          (let ((exists
                 (and name
                      (multiple-value-bind
                            (okay err/dev inode orig-mode)
                          (sb-unix:unix-stat name)
                        (declare (ignore inode)
                                 (type (or sb-int:index null) orig-mode))
                        (cond
                          (okay
                           (when (and output (= (logand orig-mode #o170000)
                                                #o40000))
                             (error 'sb-int:simple-file-error
                                 :pathname pathname
                                 :format-control
                                 "Cannot open ~S for output: Is a directory."
                                 :format-arguments (list name)))
                           (setf mode (logand orig-mode #o777))
                           t)
                          ((eql err/dev sb-unix:enoent)
                           nil)
                          (t
                           (error 'sb-int:simple-file-error
                                  :pathname pathname
                                  :format-control "Cannot find ~S: ~A"
                                  :format-arguments
                                    (list name
                                      (sb-int:strerror err/dev)))))))))
            (unless (and exists
                         (rename-file name original))
              (setf original nil)
              (setf delete-original nil)
              ;; In order to use SUPERSEDE instead, we have
              ;; to make sure unix:o_creat corresponds to
              ;; if-does-not-exist.  unix:o_creat was set
              ;; before because of if-exists being :rename.
              (unless (eq if-does-not-exist :create)
                (setf mask (logior (logandc2 mask sb-unix:o_creat)
                                   sb-unix:o_trunc)))
              (setf if-exists :supersede))))

        ;; Okay, now we can try the actual open.
        (loop
          (multiple-value-bind (fd errno)
              (if name
                  (sb-unix:unix-open name mask mode)
                  (values nil #-win32 sb-unix:enoent
                              #+win32 sb-win32::error_file_not_found))
            (cond ((integerp fd)
                   (when (eql if-exists :append)
                     (sb-unix:unix-lseek fd 0 sb-unix:l_xtnd))
                   (return (values fd name original delete-original)))
                  ((eql errno #-win32 sb-unix:enoent
                              #+win32 sb-win32::error_file_not_found)
                   (case if-does-not-exist
                     (:error
                      (cerror "Return NIL."
                              'sb-int:simple-file-error
                              :pathname pathname
                              :format-control "Error opening ~S, ~A."
                              :format-arguments
                              (list pathname
                                    (sb-int:strerror errno))))
                     (:create
                      (cerror "Return NIL."
                              'sb-int:simple-file-error
                              :pathname pathname
                              :format-control
                              "Error creating ~S, path does not exist."
                              :format-arguments (list pathname))))
                   (return nil))
                  ((eql errno #-win32 sb-unix:eexist
                              #+win32 sb-win32::error_file_not_found)
                   (unless (eq nil if-exists)
                     (cerror "Return NIL."
                             'sb-int:simple-file-error
                             :pathname pathname
                             :format-control "Error opening ~S, ~A."
                             :format-arguments
                             (list pathname
                                   (sb-int:strerror errno))))
                   (return nil))
                  #+nil ; FIXME: reinstate this; error reporting is nice.
                  ((eql errno sb-unix:eacces)
                   (cerror "Try again."
                           'sb-int:simple-file-error
                           :pathname pathname
                           :format-control "Error opening ~S, ~A."
                           :format-arguments
                           (list pathname
                                 (sb-int:strerror errno))))
                  (t
                   (cerror "Return NIL."
                           'sb-int:simple-file-error
                           :pathname pathname
                           :format-control "Error opening ~S, ~A."
                           :format-arguments
                           (list pathname
                                 (sb-int:strerror errno)))
                   (return nil)))))))))

(defun open-fd-stream (pathname &key (class 'sb-sys:fd-stream)
                                (direction :input)
                                (element-type 'base-char)
                                (if-exists nil if-exists-given)
                                (if-does-not-exist nil if-does-not-exist-given)
                                (external-format :default))
  (declare (type (or pathname string stream) pathname)
           (type (member :input :output :io :probe) direction)
           (type (member :error :new-version :rename :rename-and-delete
                         :overwrite :append :supersede nil) if-exists)
           (type (member :error :create nil) if-does-not-exist))
  (let ((filespec (merge-pathnames pathname)))
    (multiple-value-bind (fd namestring original delete-original)
        (%fd-open filespec direction if-exists if-exists-given
                  if-does-not-exist if-does-not-exist-given)
      (when fd
        (case direction
          ((:input :output :io)
           (sb-sys:make-fd-stream fd
                                  :class class
                                  :input (member direction '(:input :io))
                                  :output (member direction '(:output :io))
                                  :element-type element-type
                                  :file namestring
                                  :original original
                                  :delete-original delete-original
                                  :pathname pathname
                                  :dual-channel-p nil
                                  :input-buffer-p t
                                  :auto-close t
                                  :external-format external-format))
          (:probe
           (let ((stream (sb-impl::%make-fd-stream :name namestring :fd fd
                                                   :pathname pathname
                                                   :element-type element-type)))
             (close stream)
             stream)))))))


;; Experimental "filespec" stuff

;; sat: Hooks to parse URIs etc apparently go here

(defstruct (filespec-parser
             (:constructor make-filespec-parser (name priority function)))
  name
  priority
  function)

(defvar *filespec-parsers* ())

(defun add-filespec (name priority function)
  (let ((filespec (make-filespec-parser name priority function)))
    (setf *filespec-parsers*
          (stable-sort (cons filespec (delete name *filespec-parsers*
                                              :key #'filespec-parser-name))
                       #'>
                       :key #'filespec-parser-priority)))
  t)

(defmacro define-filespec (name lambda-list &body body)
  (let ((truename (if (consp name) (first name) name))
        (priority (if (consp name) (second name) 0)))
    `(add-filespec ',truename ,priority (lambda ,lambda-list
                                          (block ,truename
                                            ,@body)))))

(defun parse-filespec (string &optional (errorp t))
  (dolist (i *filespec-parsers* (when errorp
                                  (error "~S not recognised." string)))
    (let ((result (ignore-errors
                    (funcall (filespec-parser-function i) string))))
      (when result (return result)))))

(define-filespec pathname (string)
  (pathname string))

