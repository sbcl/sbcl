;;; -*- lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

(in-package "SB-SIMPLE-STREAMS")

;;; (pushnew :sb-simple-stream *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or X86) (pushnew :little-endian *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-little-endian (pushnew :big-endian *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or big-endian little-endian) (error "Unsupported architecture"))


;;;
;;; TYPES FOR BUFFER AND STRATEGY FUNCTIONS
;;;

;;; See chapter
;;; 12.2 Strategy descriptions necessary for encapsulation
;;; in the Franz documentation for a description of the j-xxx-fn slots.

(deftype simple-stream-buffer ()
  '(or sb-sys:system-area-pointer (sb-kernel:simple-unboxed-array (*))))

(deftype blocking ()
  `(member t nil :bnb))

(deftype j-listen-fn ()
  '(function (simple-stream) boolean))

(deftype j-read-char-fn ()
  '(function (simple-stream boolean t boolean) t)) ; may return EOF-VALUE

(deftype j-read-chars-fn ()
  '(function (simple-stream string (or character null) fixnum fixnum blocking)
	     (values fixnum &optional (member nil t :eof))))

(deftype j-write-char-fn ()
  '(function (character simple-stream) character))

(deftype j-write-chars-fn ()
  '(function (string simple-stream fixnum fixnum) t)) ;; return chars-written?

(deftype j-unread-char-fn ()
  '(function (simple-stream t) t)) ;; "relaxed" arg is boolean?  what return?

;;;
;;; STREAM CLASSES
;;;

;;; KLUDGE (sat 2003-01-15): def-stream-class and the
;;; with-stream-class / sm accessors implement a form of "sealing" of
;;; classes -- i.e., implementing very fast slot access at the price
;;; of not being able to change the class definition at runtime.
;;; Instead of a method call, a slot access for a simple-stream
;;; subclass is a funcall or (when the def-stream-class form has a
;;; location argument for the slot) a sb-pcl::clos-slots-ref.  Given a
;;; sufficiently advanced PCL with (non-standard) sealing
;;; declarations, this machinery would be superfluous.  For the time
;;; being, replacing 4 method calls with vector accesses for the fast
;;; path of read-char seems worthwhile to me.  Besides, it's the
;;; documented interface to simple-stream internals, and so it's worth
;;; keeping.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type hash-table *slot-access-functions*))
  (defvar *slot-access-functions* (make-hash-table))
  (defvar *automagic-accessors* nil))

(defmacro def-stream-class (name superclasses slots &rest options)
  (let ((accessors ())
	(real-slots ()))
    (dolist (slot slots)
      ;; Frob the slot arguments, memorizing either the location (an
      ;; integer) or the accessor of the slot.  Optionally construct
      ;; an accessor if none is given.
      (cond ((and (consp slot) (getf (rest slot) 'sb-pcl::location))
             ;; We have got a location specifier.  Memorize it and
             ;; extract it until pcl itself can work with these.
	     (push (cons (first slot)
			 (cons (getf (rest slot) :type t)
			       (getf (rest slot) 'sb-pcl::location)))
		   accessors)
	     (let ((slot (copy-list slot)))
	       (remf (rest slot) 'sb-pcl::location) ; until PCL accepts this
	       (push slot real-slots)))
	    ((or (not (consp slot)) (not (getf (rest slot) :accessor)))
	     (if *automagic-accessors*
                 ;; Add an :accessor argument, and memorize it.  FIXME:
                 ;; will this work with sbcl?  reader/writers are
                 ;; named differently there (see
                 ;; src/pcl/slot-name.lisp)
		 (let* ((slot (if (consp slot) slot (list slot)))
			(accessor (or (cdr (gethash (first slot)
						    *slot-access-functions*))
				      (intern (format nil "~A ~A slot ACCESSOR"
						      name (first slot))
					      "SB-SLOT-ACCESSOR-NAME"))))
		   (push (cons (first slot)
			       (cons (getf (rest slot) :type t) accessor))
			 accessors)
		   (push (append slot `(:accessor ,accessor)) real-slots))
		 (push slot real-slots)))
	    (t
             ;; No location given, but we have an accessor.  Memorize it.
	     (push (cons (first slot)
			 (cons (getf (rest slot) :type t)
			       (getf (rest slot) :accessor)))
		   accessors)
	     (push slot real-slots))))
    `(prog1
	 (defclass ,name ,superclasses ,(nreverse real-slots) ,@options)
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 ,@(loop for accessor in accessors
	      do (let ((exists (gethash (car accessor)
					*slot-access-functions*)))
		   (when (and exists
			      (integerp (cdr exists))
			      (integerp (cddr accessor))
			      (/= (cdr exists) (cddr accessor)))
		     (warn "~S slot ~S has moved!  ~
			    I hope you know what you're doing!"
			   name (car accessor))))
	      collect `(setf (gethash ',(car accessor) *slot-access-functions*)
			     ',(cdr accessor)))))))

(def-stream-class simple-stream (standard-object stream)
  ((plist :initform nil :type list :accessor stream-plist sb-pcl::location 19)

   ;; Strategy slots.  See section 12.2 of streams.htm for function
   ;; signatures and possible side-effects.

   ;; A function that determines if one character can be successfully
   ;; read from stream.
   (j-listen :type j-listen-fn sb-pcl::location 18)
   ;; A function that reads one character.
   (j-read-char :type j-read-char-fn sb-pcl::location 17)
   ;; A function that reads characters into a string.
   (j-read-chars :type j-read-chars-fn sb-pcl::location 16)
   ;; A function that writes one character.
   (j-write-char :type j-write-char-fn sb-pcl::location 15)
   ;; A function that writes characters from a string into the stream.
   (j-write-chars :type j-write-chars-fn sb-pcl::location 14)
   ;; A function that unreads the last character read.
   (j-unread-char :type j-unread-char-fn sb-pcl::location 13)

   ;; Other slots

   ;; Always a stream, allowing for composing external formats (see
   ;; streams.htm, section 12.5) TODO: document this better
   (melded-stream sb-pcl::location 12)
   ;; Always a stream, allowing for composing external formats (see
   ;; streams.htm, section 12.5) TODO: document this better
   (melding-base sb-pcl::location 11)
   ;; Number of octets the last read-char operation consumed TODO:
   ;; document this better; what is the difference to
   ;; last-char-read-size ?
   (encapsulated-char-read-size :initform 0 :type fixnum sb-pcl::location 10)
   (mode :initform 0 :type fixnum sb-pcl::location 9)
   (control-in :initform nil :type (or null simple-vector)
	       sb-pcl::location 8)
   (control-out :initform nil :type (or null simple-vector)
		sb-pcl::location 7)
   ;; A fixnum (denoting a valid file descriptor), a stream, or nil if
   ;; the stream is not open for input.
   (input-handle :initform nil :initarg :input-handle sb-pcl::location 6
		 :type (or null fixnum stream)
		 :accessor stream-input-handle)
   ;; A fixnum (denoting a valid file descriptor), a stream, or nil if
   ;; the stream is not open for output.
   (output-handle :initform nil :initarg :output-handle sb-pcl::location 5
		  :type (or null fixnum stream)
		  :accessor stream-output-handle)
   (external-format :initform :default sb-pcl::location 4)
   (record-end :initform nil :type (or null fixnum) sb-pcl::location 3)
   ;; The character position of the stream.
   (charpos :initform 0 :type (or null integer) sb-pcl::location 2)
   ;; Number of octets the last read-char operation consumed
   (last-char-read-size :initform 0 :type fixnum sb-pcl::location 1)
   ;; instance flags (not a normal slot in Allegro CL)
   (%flags :initform 0 :type fixnum sb-pcl::location 0)))

(def-stream-class probe-simple-stream (simple-stream)
  ())

;;; A stream with a single buffer, for example a file stream.
(def-stream-class single-channel-simple-stream (simple-stream)
  ;; Input/output buffer.
  ((buffer :initform nil :type (or simple-stream-buffer null)
           sb-pcl::location 23)
   ;; Current position in buffer.
   (buffpos :initform 0 :type fixnum sb-pcl::location 22)
   ;; Maximum valid position in buffer, or -1 on eof.
   (buffer-ptr :initform 0 :type fixnum sb-pcl::location 21)
   (buf-len :initform 0 :type fixnum sb-pcl::location 20)))

(def-stream-class direct-simple-stream (single-channel-simple-stream)
  ())

(def-stream-class buffer-input-simple-stream (direct-simple-stream)
  ())

(def-stream-class buffer-output-simple-stream (direct-simple-stream)
  ((out-buffer :initform nil :type (or simple-stream-buffer null)
	       sb-pcl::location 26)
   ;; Current position in output buffer.
   (outpos :initform 0 :type fixnum sb-pcl::location 25)
   ;; Buffer length (one greater than maximum output buffer index)
   (max-out-pos :initform 0 :type fixnum sb-pcl::location 24)))

(def-stream-class null-simple-stream (single-channel-simple-stream)
  ())

(def-stream-class file-simple-stream (single-channel-simple-stream)
  ((pathname :initform nil :initarg :pathname)
   (filename :initform nil :initarg :filename)
   (original :initform nil :initarg :original)
   (delete-original :initform nil :initarg :delete-original)
   ))

(def-stream-class mapped-file-simple-stream (file-simple-stream
					     direct-simple-stream)
  ())

;;; A stream with two octet buffers, for example a socket or terminal
;;; stream.
(def-stream-class dual-channel-simple-stream (simple-stream)
  ;; Output buffer.
  ((out-buffer :initform nil :type (or simple-stream-buffer null)
	       sb-pcl::location 26)
   ;; Current position in output buffer.
   (outpos :initform 0 :type fixnum sb-pcl::location 25)
   ;; Buffer length (one greater than maximum output buffer index)
   (max-out-pos :initform 0 :type fixnum sb-pcl::location 24)
   ;; Input buffer (in this case; the 'buffer' slot serves as
   ;; bidirectional buffer for single-channel-simple-streams).
   (buffer :initform nil :type (or simple-stream-buffer null)
           sb-pcl::location 23)
   ;; Current position in buffer.
   (buffpos :initform 0 :type fixnum sb-pcl::location 22)
   ;; Maximum valid position in buffer, or -1 on eof.
   (buffer-ptr :initform 0 :type fixnum sb-pcl::location 21)
   (buf-len :initform 0 :type fixnum sb-pcl::location 20)))

(def-stream-class terminal-simple-stream (dual-channel-simple-stream)
  ())

(def-stream-class socket-simple-stream (dual-channel-simple-stream)
  ((socket :initform nil :type (or sb-bsd-sockets:socket null)
           :initarg :socket sb-pcl::location 27)))

(def-stream-class socket-base-simple-stream (dual-channel-simple-stream)
  ())

(def-stream-class hiper-socket-simple-stream (dual-channel-simple-stream)
  ())

;;; A stream with a string as buffer.
(def-stream-class string-simple-stream (simple-stream)
  ;; The input/output buffer.
  ((buffer :initform nil :type (or simple-stream-buffer null)
           sb-pcl::location 23)
   ;; Current position in buffer.
   (buffpos :initform 0 :type fixnum sb-pcl::location 22)
   ;; Maximum valid position in buffer, or -1 on eof.
   (buffer-ptr :initform 0 :type fixnum sb-pcl::location 21)
   (buf-len :initform 0 :type fixnum sb-pcl::location 20)))

(def-stream-class composing-stream (string-simple-stream)
  ())

(def-stream-class string-input-simple-stream (string-simple-stream)
  ())

(def-stream-class string-output-simple-stream (string-simple-stream)
  ;; The output buffer (slot added so that a class can inherit from
  ;; both string-input-simple-stream and string-output-simple-stream
  ;; without the strategies clashing)
  ((out-buffer :initform nil :type (or simple-stream-buffer null)
	       sb-pcl::location 26)
   ;; Current position in output buffer.
   (outpos :initform 0 :type fixnum sb-pcl::location 25)
   ;; Buffer length (one greater than maximum output buffer index)
   (max-out-pos :initform 0 :type fixnum sb-pcl::location 24)))

(def-stream-class fill-pointer-output-simple-stream
    (string-output-simple-stream)
  ())

(def-stream-class limited-string-output-simple-stream
    (string-output-simple-stream)
  ())

(def-stream-class xp-simple-stream (string-output-simple-stream)
  ())

(def-stream-class annotation-output-simple-stream (string-output-simple-stream)
  ())


(defclass default-latin1-base-ef () ())
(defclass stream-recording-mixin () ())
(defclass stream-recording-repaint-mixin () ())


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *automagic-accessors* nil))

;;;
;;; DEVICE-LEVEL FUNCTIONS
;;;

(defgeneric device-open (stream options))

(defgeneric device-close (stream abort))

(defgeneric device-buffer-length (stream))

(defgeneric device-file-position (stream))

(defgeneric (setf device-file-position) (value stream))

(defgeneric device-file-length (stream))

(defgeneric device-read (stream buffer start end blocking))

(defgeneric device-clear-input (stream buffer-only))

(defgeneric device-write (stream buffer start end blocking))

(defgeneric device-clear-output (stream))

(defgeneric device-extend (stream need action))

(defgeneric device-finish-record (stream blocking action))
