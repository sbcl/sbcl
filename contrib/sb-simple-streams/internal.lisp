;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: STREAM -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

(in-package "SB-SIMPLE-STREAMS")

;;;
;;; HELPER FUNCTIONS
;;;

;; All known stream flags.  Note that the position in the constant
;; list is significant (cf. %flags below).
(sb-int:defconstant-eqx +flag-bits+
                        '(:simple       ; instance is valid
                          :input :output ; direction
                          :dual :string	; type of stream
                          :eof          ; latched EOF
                          :dirty        ; output buffer needs write
                          :interactive) ; interactive stream
                        #'equal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flags (flags)
    (loop for flag in flags
	  as pos = (position flag +flag-bits+)
	when (eq flag :gray) do
	  (error "Gray streams not supported.")
	if pos
	  sum (ash 1 pos) into bits
	else
	  collect flag into unused
      finally (when unused
		(warn "Invalid stream instance flag~P: ~{~S~^, ~}"
		      (length unused) unused))
	      (return bits))))

;;; Setup an environment where sm, funcall-stm-handler and
;;; funcall-stm-handler-2 are valid and efficient for a stream of type
;;; class-name or for the stream argument (in which case the
;;; class-name argument is ignored).  In nested with-stream-class
;;; forms, the inner with-stream-class form must specify a stream
;;; argument if the outer one specifies one, or the wrong object will
;;; be accessed.
(defmacro with-stream-class ((class-name &optional stream) &body body)
  (if stream
      (let ((stm (gensym "STREAM"))
	    (slt (gensym)))
	`(let* ((,stm ,stream)
		(,slt (sb-pcl::std-instance-slots ,stm)))
	   (declare (type ,class-name ,stm) (ignorable ,slt))
	   (macrolet ((sm (slot-name stream)
			(declare (ignore stream))
			(let ((slot-access (gethash slot-name
						    *slot-access-functions*)))
			  (cond ((sb-int:fixnump (cdr slot-access))
                                 ;; Get value in nth slot
				 `(the ,(car slot-access)
				    (sb-pcl::clos-slots-ref ,',slt
                                                            ,(cdr slot-access))))
				(slot-access
                                 ;; Call memorized function
				 `(the ,(car slot-access) (,(cdr slot-access)
							    ,',stm)))
				(t
                                 ;; Use slot-value
                                 `(slot-value ,',stm ',slot-name)))))
		      (add-stream-instance-flags (stream &rest flags)
			(declare (ignore stream))
			`(setf (sm %flags ,',stm) (logior (sm %flags ,',stm)
							  ,(%flags flags))))
		      (remove-stream-instance-flags (stream &rest flags)
			(declare (ignore stream))
			`(setf (sm %flags ,',stm) (logandc2 (sm %flags ,',stm)
							    ,(%flags flags))))
		      (any-stream-instance-flags (stream &rest flags)
			(declare (ignore stream))
			`(not (zerop (logand (sm %flags ,',stm)
					     ,(%flags flags))))))
	     ,@body)))
      `(macrolet ((sm (slot-name stream)
		    (let ((slot-access (gethash slot-name
						*slot-access-functions*)))
		      (cond ((sb-int:fixnump (cdr slot-access))
			     `(the ,(car slot-access)
				(sb-pcl::clos-slots-ref
				 (sb-pcl::std-instance-slots ,stream)
				 ,(cdr slot-access))))
			    (slot-access
			     `(the ,(car slot-access) (,(cdr slot-access)
							,stream)))
			    (t `(slot-value ,stream ',slot-name))))))
	 ,@body)))

(defmacro sm (slot-name stream)
  (let ((slot-access (gethash slot-name *slot-access-functions*)))
    (warn "Using ~S macro outside ~S" 'sm 'with-stream-class)
    (cond ((sb-int:fixnump (cdr slot-access))
	   `(the ,(car slot-access) (sb-pcl::clos-slots-ref
				     (sb-pcl::std-instance-slots ,stream)
				     ,(cdr slot-access))))
	  (slot-access
	   `(the ,(car slot-access) (,(cdr slot-access) ,stream)))
	  (t `(slot-value ,stream ',slot-name)))))

(defmacro funcall-stm-handler (slot-name stream &rest args)
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (funcall (sm ,slot-name ,s) ,s ,@args))))

(defmacro funcall-stm-handler-2 (slot-name arg1 stream &rest args)
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (funcall (sm ,slot-name ,s) ,arg1 ,s ,@args))))

(defmacro add-stream-instance-flags (stream &rest flags)
  "Set the given flag bits in STREAM."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (setf (sm %flags ,s) (logior (sm %flags ,s) ,(%flags flags)))))))

(defmacro remove-stream-instance-flags (stream &rest flags)
  "Clear the given flag bits in STREAM."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (setf (sm %flags ,s) (logandc2 (sm %flags ,s) ,(%flags flags)))))))

(defmacro any-stream-instance-flags (stream &rest flags)
  "Determine whether any one of the FLAGS is set in STREAM."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
	 (not (zerop (logand (sm %flags ,s) ,(%flags flags))))))))


(declaim (inline buffer-sap bref (setf bref) buffer-copy))

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
  (sb-sys:sap-ref-8 (buffer-sap buffer) index))

(defun (setf bref) (octet buffer index)
  (declare (type (unsigned-byte 8) octet)
	   (type simple-stream-buffer buffer)
	   (type (integer 0 #.most-positive-fixnum) index))
  (setf (sb-sys:sap-ref-8 (buffer-sap buffer) index) octet))

(defun buffer-copy (src soff dst doff length)
  (declare (type simple-stream-buffer src dst)
	   (type fixnum soff doff length))
  (sb-sys:without-gcing ;; is this necessary??
   (sb-kernel:system-area-copy (buffer-sap src) (* soff 8)
                               (buffer-sap dst) (* doff 8)
                               (* length 8))))

(defun allocate-buffer (size)
  (if (= size sb-impl::bytes-per-buffer)
      (sb-impl::next-available-buffer)
      (make-array size :element-type '(unsigned-byte 8))))

(defun free-buffer (buffer)
  (when (not (vectorp buffer))
    (push buffer sb-impl::*available-buffers*))
  t)

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
    (let ((name (cond ((sb-int:unix-namestring pathname input))
		      ((and input (eq if-does-not-exist :create))
		       (sb-int:unix-namestring pathname nil)))))
      ;; Process if-exists argument if we are doing any output.
      (cond (output
	     (unless if-exists-given
	       (setf if-exists
		     (if (eq (pathname-version pathname) :newest)
			 :new-version
			 :error)))
	     (case if-exists
	       ((:error nil)
		(setf mask (logior mask sb-unix:o_excl)))
	       ((:rename :rename-and-delete)
		(setf mask (logior mask sb-unix:o_creat)))
	       ((:new-version :supersede)
		(setf mask (logior mask sb-unix:o_trunc)))
	       (:append
		(setf mask (logior mask sb-unix:o_append)))))
	    (t
	     (setf if-exists nil)))	; :ignore-this-arg
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
		  (values nil sb-unix:enoent))
	    (cond ((sb-int:fixnump fd)
		   (return (values fd name original delete-original)))
		  ((eql errno sb-unix:enoent)
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
		  ((eql errno sb-unix:eexist)
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

(defun open-fd-stream (pathname &key (direction :input)
				(element-type 'base-char)
				(if-exists nil if-exists-given)
				(if-does-not-exist nil if-does-not-exist-given)
				(external-format :default))
  (declare (type (or pathname string stream) pathname)
	   (type (member :input :output :io :probe) direction)
	   (type (member :error :new-version :rename :rename-and-delete
			 :overwrite :append :supersede nil) if-exists)
	   (type (member :error :create nil) if-does-not-exist)
	   (ignore external-format))
  (setq pathname (pathname pathname))
  (multiple-value-bind (fd namestring original delete-original)
      (%fd-open pathname direction if-exists if-exists-given
		if-does-not-exist if-does-not-exist-given)
    (when fd
      (case direction
	((:input :output :io)
	 (sb-sys:make-fd-stream fd
                                :input (member direction '(:input :io))
                                :output (member direction '(:output :io))
                                :element-type element-type
                                :file namestring
                                :original original
                                :delete-original delete-original
                                :pathname pathname
                                :input-buffer-p t
                                :auto-close t))
	(:probe
	 (let ((stream (sb-impl::%make-fd-stream :name namestring :fd fd
                                                 :pathname pathname
                                                 :element-type element-type)))
	   (close stream)
	   stream))))))


;; Make PATHNAME and NAMESTRING work
(defun cl::file-name (stream &optional new-name)
  (typecase stream
    (file-simple-stream
     (with-stream-class (file-simple-stream stream)
       (cond (new-name
	      (setf (sm pathname stream) new-name)
	      (setf (sm filename stream) (sb-int:unix-namestring new-name nil))
	      t)
	     (t
	      (sm pathname stream)))))
    (sb-sys::file-stream
     (cond (new-name
	    (setf (sb-impl::fd-stream-pathname stream) new-name)
	    (setf (sb-impl::fd-stream-file stream)
		  (sb-int:unix-namestring new-name nil))
	    t)
	   (t
	    (sb-impl::fd-stream-pathname stream))))))

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
