;;;; code for string to octet conversion

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; FIXME: The latin9 stuff is currently #!+sb-unicode, because I
;;; don't like the idea of trying to do CODE-CHAR #x<big>.  Is that a
;;; justified fear?  Can we arrange that it's caught and converted to
;;; a decoding error error?  Or should we just give up on non-Unicode
;;; builds?

(in-package "SB!IMPL")

;;; FIXME: don't we have this somewhere else?
(deftype array-range ()
  "A number that can represent an index into a vector, including
one-past-the-end"
  '(integer 0 #.sb!xc:array-dimension-limit))

;;;; conditions

;;; encoding condition

(define-condition octets-encoding-error (character-encoding-error)
  ((string :initarg :string :reader octets-encoding-error-string)
   (position :initarg :position :reader octets-encoding-error-position)
   (external-format :initarg :external-format
                    :reader octets-encoding-error-external-format))
  (:report (lambda (c s)
	     (format s "Unable to encode character ~A as ~S."
		     (char-code (char (octets-encoding-error-string c)
				      (octets-encoding-error-position c)))
		     (octets-encoding-error-external-format c)))))
		     
(defun read-replacement-character ()
  (format *query-io*
          "Replacement byte, bytes, character, or string (evaluated): ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun encoding-error (external-format string pos)
  (restart-case
      (error 'octets-encoding-error
	     :external-format external-format
	     :string string
	     :position pos)
    (use-value (replacement)
      :report "Supply a set of bytes to use in place of the invalid one."
      :interactive read-replacement-character
      (typecase replacement
	((unsigned-byte 8)
	 (make-array 1 :element-type '(unsigned-byte 8) :initial-element replacement))
	(character
	 (string-to-octets (string replacement)
                           :external-format external-format))
	(string
	 (string-to-octets replacement
			   :external-format external-format))
	(t
	 (coerce replacement '(simple-array (unsigned-byte 8) (*))))))))

;;; decoding condition

;;; for UTF8, the specific condition signalled will be a generalized
;;; instance of one of the following:
;;;
;;;   end-of-input-in-character
;;;   character-out-of-range
;;;   invalid-utf8-starter-byte
;;;   invalid-utf8-continuation-byte
;;;   overlong-utf8-sequence
;;;
;;; Of these, the only one truly likely to be of interest to calling
;;; code is end-of-input-in-character (in which case it's likely to
;;; want to make a note of octet-decoding-error-start, supply "" as a
;;; replacement string, and then move that last chunk of bytes to the
;;; beginning of its buffer for the next go round) but they're all
;;; provided on the off chance they're of interest.  The next most
;;; likely interesting option is overlong-utf8-sequence -- the
;;; application, if it cares to, can decode this itself (taking care
;;; to ensure that the result isn't out of range of CHAR-CODE-LIMIT)
;;; and return that result.  This library doesn't provide support for
;;; that as a conforming UTF-8-using program is supposed to treat it
;;; as an error.

(define-condition octet-decoding-error (character-decoding-error)
  ((array :initarg :array :accessor octet-decoding-error-array)
   (start :initarg :start :accessor octet-decoding-error-start)
   (end :initarg :end :accessor octet-decoding-error-end)
   (position :initarg :pos :accessor octet-decoding-bad-byte-position)
   (external-format :initarg :external-format
                    :accessor octet-decoding-error-external-format))
  (:report
   (lambda (condition stream)
     (format stream "Illegal ~S character starting at byte position ~D."
             (octet-decoding-error-external-format condition)
             (octet-decoding-error-start condition)))))

(define-condition end-of-input-in-character (octet-decoding-error) ())
(define-condition character-out-of-range (octet-decoding-error) ())
(define-condition invalid-utf8-starter-byte (octet-decoding-error) ())
(define-condition invalid-utf8-continuation-byte (octet-decoding-error) ())
(define-condition overlong-utf8-sequence (octet-decoding-error) ())

(define-condition malformed-ascii (octet-decoding-error) ())

(defun read-replacement-string ()
  (format *query-io* "Enter a replacement string designator (evaluated): ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun decoding-error (array start end external-format reason pos)
  (restart-case
      (error reason
	     :external-format external-format
	     :array array
	     :start start
	     :end end
	     :pos pos)
    (use-value (s)
      :report "Supply a replacement string designator."
      :interactive read-replacement-string
      (string s))))

;;; overflow on caller-supplied-replacement condition

(define-condition octet-buffer-overflow (condition)
  ((replacement :initarg :replacement :accessor octet-buffer-overflow-replacement)))

(defun overflow (s)
  (with-simple-restart (continue "Keep processing, not invoking outer handlers.")
    (signal 'octet-buffer-overflow :replacement s)))

;;; Utilities used in both to-string and to-octet conversions

(defmacro instantiate-octets-definition (definer)
  `(progn
    (,definer aref (simple-array (unsigned-byte 8) (*)))
    (,definer sap-ref-8 system-area-pointer)))

;;; maps into TO-SEQ from elements of FROM-SEQ via MAPPER.  MAPPER
;;; returns two values: the number of elments stored in TO-SEQ, and
;;; the number used up from FROM-SEQ.  MAPPER is responsible for
;;; getting out if either sequence runs out of room.
(declaim (inline varimap))
(defun varimap (to-seq to-start to-end from-start from-end mapper)
  (declare (optimize speed (safety 0))
	   (type array-range to-start to-end from-start from-end)
	   (type function mapper))
  (loop with from-size of-type array-range = 0
	and to-size of-type array-range = 0
	for to-pos of-type array-range = to-start then (+ to-pos to-size)
	for from-pos of-type array-range = from-start then (+ from-pos from-size)
	while (and (< to-pos to-end)
		   (< from-pos from-end))
	do (multiple-value-bind (ts fs) (funcall mapper to-pos from-pos)
	     (setf to-size ts
		   from-size fs))
	finally (return (values to-seq to-pos from-pos))))

;;; FIXME: find out why the comment about SYMBOLICATE below is true
;;; and fix it, or else replace with SYMBOLICATE.
;;;
;;; FIXME: this is cute, but is going to prevent greps for def.*<name>
;;; from working for (defun ,(make-od-name ...) ...)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-od-name (sym1 sym2)
    ;; "MAKE-NAME" is too generic, but this doesn't do quite what
    ;; SYMBOLICATE does; MAKE-OD-NAME ("octets definition") it is
    ;; then.
    (intern (concatenate 'string (symbol-name sym1) "-" (symbol-name sym2))
	    (symbol-package sym1))))

(defmacro define-replace-into-notseq (accessor type)
  (declare (ignore type))
  (let ((name (make-od-name 'replace-into-notseq accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (dest src dest-start)
	(declare (optimize speed (safety 0)))
	;; Known: all of SRC (which is a SEQ) fits into DEST
	(if (listp src)
	    (loop for srcobj in src
		  for idx of-type array-range from dest-start
		  do (setf (,accessor dest idx) srcobj))
	    (loop for srcidx of-type array-range below (length src)
		  for destidx of-type array-range from dest-start
		  do (setf (,accessor dest destidx) (aref src srcidx))))
	dest))))
(instantiate-octets-definition define-replace-into-notseq)

(defmacro define-vari-transcode (accessor type)
  (declare (ignore type))
  (let ((name (make-od-name 'vari-transcode accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name
	  (to to-start to-end from from-start from-end replacements getter elementp)
	(declare (optimize speed (safety 0))
		 (type array-range to-start to-end from-start from-end)
		 (type function getter elementp))
	;; convert from FROM to TO via the mapping function GETTER
	;; which can return either a single element for TO or a
	;; sequence of elments; in the latter case the sequence is
	;; taken from the head of the (boxed) list REPLACEMENTS.
	;; ELEMENTP tests to see which of the two return types was
	;; received.
	(let ((replacements-box (if replacements (cons nil replacements) nil)))
	  (declare (dynamic-extent replacements-box))
	  (varimap to to-start to-end
		   from-start from-end
		   (lambda (to-pos from-pos)
		     (multiple-value-bind (element-or-vector used-from)
			 (funcall getter from from-pos from-end replacements-box)
		       (cond
			 ((funcall elementp element-or-vector)
			  (setf (,accessor to to-pos) element-or-vector)
			  (values 1 used-from))
			 ((> (+ to-pos (length element-or-vector)) to-end)
			  (overflow element-or-vector)
			  (return-from ,name (values to to-pos from-pos)))
			 (t
			  (,(make-od-name 'replace-into-notseq accessor) to element-or-vector to-pos)
			  (values (length element-or-vector) used-from)))))))))))

(instantiate-octets-definition define-vari-transcode)

;;;; to-octets conversions

;;; to latin (including ascii)
(defmacro define-string->latin*% (accessor type)
  (let ((name (make-od-name 'string->latin*% accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (array astart aend string sstart send replacements get-bytes)
	(declare (optimize speed (safety 0))
		 (type simple-string string)
		 (type ,type array))
	(,(make-od-name 'vari-transcode accessor)
	  array astart aend
	  string sstart send
	  replacements
	  get-bytes
	  #'numberp)))))
(instantiate-octets-definition define-string->latin*%)

(declaim (inline get-ascii-bytes))
(defun get-ascii-bytes (string pos end replacements-box)
  (declare (ignore end))
  (let ((code (char-code (char string pos))))
    (values (cond
              ((< code 128) code)
              ((null replacements-box)
               (encoding-error :ascii string pos))
              (t (pop (cdr replacements-box))))
            1)))

(declaim (inline get-latin-bytes))
(defun get-latin-bytes (mapper external-format string pos end replacements-box)
  (declare (ignore end))
  (let ((code (funcall mapper (char-code (char string pos)))))
    (values (cond
	      ((< code 256) code)
	      ((null replacements-box)
	       (encoding-error external-format string pos))
	      (t
	       (pop (cdr replacements-box))))
	    1)))

#!+sb-unicode
(progn
  (declaim (inline code->latin9-mapper))
  (defun code->latin9-mapper (code)
    (declare (optimize speed (safety 0))
             (type char-code code))
    (case code
      (#x20AC #xA4)
      (#x0160 #xA6)
      (#x0161 #xA8)
      (#x017D #xB4)
      (#x017E #xB8)
      (#x0152 #xBC)
      (#x0153 #xBD)
      (#x0178 #xBE)
      (otherwise code))))

(defmacro define-string->ascii* (accessor type)
  (let ((name (make-od-name 'string->ascii* accessor)))
    `(defun ,name (array astart aend string sstart send)
      (declare (optimize speed (safety 0))
       (type ,type array)
       (type simple-string string)
       (type array-range astart aend sstart send))
      (,(make-od-name 'string->latin*% accessor)
	array astart aend
	string sstart send
	nil
	#'get-ascii-bytes))))
(instantiate-octets-definition define-string->ascii*)

(declaim (inline get-latin1-bytes))
(defun get-latin1-bytes (string pos end replacements)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range pos end))
  (get-latin-bytes #'identity :latin-1 string pos end replacements))

(defmacro define-string->latin1* (accessor type)
  (let ((name (make-od-name 'string->latin1* accessor)))
    `(defun ,name (array astart aend string sstart send)
      (declare (optimize speed (safety 0))
	       (type ,type array)
	       (type simple-string string)
	       (type array-range astart aend sstart send))
      (,(make-od-name 'string->latin*% accessor)
	array astart aend
	string sstart send
	nil
	#'get-latin1-bytes))))
(instantiate-octets-definition define-string->latin1*)

#!-sb-unicode
(progn
  (declaim (inline get-latin9-bytes))
  (defun get-latin9-bytes (string pos end replacements)
    (declare (optimize speed (safety 0))
             (type simple-string string)
             (type array-range pos end))
    (get-latin-bytes #'code->latin9-mapper :latin-9 string pos end replacements))

  (defmacro define-string->latin9* (accessor type)
    (let ((name (make-od-name 'string->latin9* accessor)))
      `(defun ,name (array astart aend string sstart send)
        (declare (optimize speed (safety 0))
         (type ,type array)
         (type simple-string string)
         (type array-range astart aend sstart send))
        (,(make-od-name 'string->latin*% accessor)
         array astart aend
         string sstart send
         nil
         #'get-latin9-bytes))))
  (instantiate-octets-definition define-string->latin9*))

(defun get-latin-length (string start end get-bytes)
  ;; Returns the length and a list of replacements for bad characters
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range start end)
	   (type function get-bytes))
  (let* ((length 0)
	 (replacements-start (cons nil nil))
	 (replacements-end replacements-start))
    (declare (dynamic-extent replacements-start)
	     (type array-range length))
    (flet ((collect (replacement)
	     (setf (cdr replacements-end) (cons replacement nil)
		   replacements-end (cdr replacements-end))
	     replacement))
      (loop for src of-type fixnum from start below end
	    do (let ((byte-or-bytes (funcall get-bytes string src end nil)))
		 (declare (type (or (unsigned-byte 8) (simple-array (unsigned-byte 8) (*))) byte-or-bytes))
		 (cond
		   ((numberp byte-or-bytes)
		    (incf length))
		   (t
		    (let* ((replacement-len (length byte-or-bytes))
			   (total-length (+ length replacement-len)))
		      (unless (< total-length #.sb!xc:array-dimension-limit)
			(error "Replacement string too long"))
		      (setf length total-length)
		      (collect byte-or-bytes)))))))
    (values length (cdr replacements-start))))

(declaim (inline string->latin%))
(defun string->latin% (string sstart send get-bytes null-padding)
  (declare (optimize speed); (safety 0))
	   (type simple-string string)
	   (type array-range sstart)
	   (type array-range send)
	   (type function get-bytes))
  (let ((octets (make-array 0 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (loop for pos from sstart below send
	  do (let ((byte-or-bytes (funcall get-bytes string pos send nil)))
	       (declare (type (or (unsigned-byte 8) (simple-array (unsigned-byte 8) (*))) byte-or-bytes))
	       (cond
		 ((numberp byte-or-bytes)
		  (vector-push-extend byte-or-bytes octets))
		 (t
		  (dotimes (i (length byte-or-bytes))
		    (vector-push-extend (aref byte-or-bytes i) octets))))))
    (dotimes (i null-padding)
      (vector-push-extend 0 octets))
    (coerce octets '(simple-array (unsigned-byte 8) (*)))))

(defun string->ascii (string sstart send null-padding)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range sstart send))
  (values (string->latin% string sstart send #'get-ascii-bytes null-padding)))

(defun string->latin1 (string sstart send null-padding)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range sstart send))
  (values (string->latin% string sstart send #'get-latin1-bytes null-padding)))

#!+sb-unicode
(defun string->latin9 (string sstart send null-padding)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range sstart send))
  (values (string->latin% string sstart send #'get-latin9-bytes null-padding)))

;;; to utf8

(declaim (inline char-len-as-utf8))
(defun char-len-as-utf8 (c)
  (declare (optimize speed (safety 0))
	   (type character c))
  (let ((code (char-code c)))
    (cond ((< code 0) (bug "can't happen"))
	  ((< code #x80) 1)
	  ((< code #x800) 2)
	  ((< code #x10000) 3)
	  ((< code #x110000) 4)
	  (t (bug "can't happen")))))

(defmacro define-char->utf8 (accessor type)
  (let ((name (make-od-name 'char->utf8 accessor)))
    `(progn
      ;;(declaim (inline ,name))
      (defun ,name (char dest destpos maxdest)
	(declare (optimize speed (safety 0))
		 (type ,type dest)
		 (type array-range destpos maxdest))
	;; stores the character in the array DEST if there's room between
	;; DESTPOS and MAXDEST.  Returns the number of bytes used on
	;; success, or NIL on failure.
	(let ((code (char-code char)))
	  (flet (((setf cref) (c pos)
		   (setf (,accessor dest (+ pos destpos)) c)))
	    (declare (inline (setf cref)))
	    (ecase (char-len-as-utf8 char)
	      (1
	       (cond ((>= destpos maxdest)
		      nil)
		     (t
		      (setf (cref 0) code)
		      1)))
	      (2
	       (cond ((>= (+ destpos 1) maxdest)
		      nil)
		     (t
		      (setf (cref 0) (logior #b11000000 (ldb (byte 5 6) code))
			    (cref 1) (logior #b10000000 (ldb (byte 6 0) code)))
		      2)))
	      (3
	       (cond ((>= (+ destpos 2) maxdest)
		      nil)
		     (t
		      (setf (cref 0) (logior #b11100000 (ldb (byte 4 12) code))
			    (cref 1) (logior #b10000000 (ldb (byte 6 6) code))
			    (cref 2) (logior #b10000000 (ldb (byte 6 0) code)))
		      3)))
	      (4
	       (cond ((>= (+ destpos 3) maxdest)
		      nil)
 		     (t
		      (setf (cref 0) (logior #b11110000 (ldb (byte 3 18) code))
			    (cref 1) (logior #b10000000 (ldb (byte 6 12) code))
			    (cref 2) (logior #b10000000 (ldb (byte 6 6) code))
			    (cref 3) (logior #b10000000 (ldb (byte 6 0) code)))
		      4))))))))))
(instantiate-octets-definition define-char->utf8)

(defmacro define-string->utf8* (accessor type)
  (let ((name (make-od-name 'string->utf8* accessor)))
    `(progn
      (defun ,name (array astart aend string sstart send)
	(declare (optimize speed (safety 0))
		 (type simple-string string)
		 (type ,type array)
		 (type array-range astart aend sstart send))
	(flet ((convert (spos apos)
		 (let ((char-len (,(make-od-name 'char->utf8 accessor) (char string spos) array apos aend)))
		   (when (not char-len)
		     (return-from ,name (values array apos spos)))
		   char-len)))
	  (varimap array astart aend
		   sstart send
		   (lambda (apos spos)
		     (values (convert spos apos) 1))))))))
(instantiate-octets-definition define-string->utf8*)

(defun string->utf8 (string sstart send additional-space)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range sstart send additional-space))
  (let ((alen (+ (the (integer 0 #.(* 4 sb!xc:array-dimension-limit))
		   (loop with result of-type array-range = 0
			 for i of-type array-range from sstart below send
			 do (incf result (char-len-as-utf8 (char string i)))
			 finally (return result)))
		 additional-space)))
    (when (>= alen #.sb!xc:array-dimension-limit)
      (error "string too long as utf8"))
    (let ((array (make-array alen :element-type '(unsigned-byte 8))))
      (when (plusp additional-space)
	(fill array 0 :start (- alen additional-space)))
      (values (string->utf8*-aref array 0 alen string sstart send)))))

;;;; to-string conversions

;;; from latin (including ascii)

(defmacro define-ascii->string* (accessor type)
  (let ((name (make-od-name 'ascii->string* accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (string sstart send array astart aend)
	(declare (optimize speed (safety 0))
		 (type simple-string string)
		 (type ,type array)
		 (type array-range sstart send astart aend))
	(varimap string sstart send
		 astart aend
		 (lambda (spos apos)
		   (setf (char string spos)
                         (let ((code (,accessor array apos)))
                           (if (< code 128)
                               code
                               (decoding-error array astart aend :ascii
                                               'malformed-ascii apos))))
		   (values 1 1)))))))
(instantiate-octets-definition define-ascii->string*)

(defmacro define-latin->string* (accessor type)
  (let ((name (make-od-name 'latin->string* accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (string sstart send array astart aend mapper)
	(declare (optimize speed (safety 0))
		 (type simple-string string)
		 (type ,type array)
		 (type array-range sstart send astart aend)
		 (function mapper))
	(varimap string sstart send
		 astart aend
		 (lambda (spos apos)
		   (setf (char string spos) (code-char (funcall mapper (,accessor array apos))))
		   (values 1 1)))))))
(instantiate-octets-definition define-latin->string*)

#!+sb-unicode
(progn
  (declaim (inline latin9->code-mapper))
  (defun latin9->code-mapper (byte)
    (declare (optimize speed (safety 0))
             (type (unsigned-byte 8) byte))
    (case byte
      (#xA4 #x20AC)
      (#xA6 #x0160)
      (#xA8 #x0161)
      (#xB4 #x017D)
      (#xB8 #x017E)
      (#xBC #x0152)
      (#xBD #x0153)
      (#xBE #x0178)
      (otherwise byte))))

(defmacro define-latin1->string* (accessor type)
  (declare (ignore type))
  (let ((name (make-od-name 'latin1->string* accessor)))
    `(progn
      (defun ,name (string sstart send array astart aend)
	(,(make-od-name 'latin->string* accessor) string sstart send array astart aend #'identity)))))
(instantiate-octets-definition define-latin1->string*)

#!+sb-unicode
(progn
  (defmacro define-latin9->string* (accessor type)
    (declare (ignore type))
    (let ((name (make-od-name 'latin9->string* accessor)))
      `(progn
        (defun ,name (string sstart send array astart aend)
          (,(make-od-name 'latin->string* accessor) string sstart send array astart aend #'latin9->code-mapper)))))
  (instantiate-octets-definition define-latin9->string*))
  
(declaim (inline ascii->string))
(defun ascii->string (array astart aend)
  (declare (optimize speed (safety 0))
	   (type (simple-array (unsigned-byte 8) (*)) array)
	   (type array-range astart aend))
  (let ((length (the array-range (- aend astart))))
    (values (ascii->string*-aref (make-string length) 0 length
				 array astart aend))))

(declaim (inline latin->string))
(defun latin->string (array astart aend mapper)
  (declare (optimize speed (safety 0))
	   (type (simple-array (unsigned-byte 8) (*)) array)
	   (type array-range astart aend)
	   (type function mapper))
  (let ((length (the array-range (- aend astart))))
    (values (latin->string*-aref (make-string length) 0 length
				 array astart aend
				 mapper))))

(defun latin1->string (array astart aend)
  (latin->string array astart aend #'identity))

#!+sb-unicode
(defun latin9->string (array astart aend)
  (latin->string array astart aend #'latin9->code-mapper))

;;; from utf8

(defmacro define-bytes-per-utf8-character (accessor type)
  (let ((name (make-od-name 'bytes-per-utf8-character accessor)))
    `(progn
      ;;(declaim (inline ,name))
      (let ((lexically-max
             (string->utf8 (string (code-char (1- #.sb!xc:char-code-limit)))
                           0 1 0)))
	(defun ,name (array pos end replacements-box)
	  (declare (optimize speed (safety 0))
		   (type ,type array)
		   (type array-range pos end))
	  ;; returns the number of bytes consumed and nil if it's a
	  ;; valid character or the number of bytes consumed and a
	  ;; replacement string if it's not.  If REPLACEMENTS is NIL,
	  ;; signal a condition to get one, otherwise pop it off the
	  ;; cdr of REPLACEMENTS.
	  (let ((initial-byte (,accessor array pos))
		(reject-reason 'no-error)
		(reject-position pos))
	    (declare (type array-range reject-position))
	    (labels ((valid-utf8-starter-byte-p (b)
		       (declare (type (unsigned-byte 8) b))
		       (let ((ok (cond
				   ((zerop (logand b #b10000000)) 1)
				   ((= (logand b #b11100000) #b11000000)
				    2)
				   ((= (logand b #b11110000) #b11100000)
				    3)
				   ((= (logand b #b11111000) #b11110000)
				    4)
				   ((= (logand b #b11111100) #b11111000)
				    5)
				   ((= (logand b #b11111110) #b11111100)
				    6)
				   (t
				    nil))))
			 (unless ok
			   (setf reject-reason 'invalid-utf8-starter-byte))
			 ok))
		     (enough-bytes-left-p (x)
		       (let ((ok (> end (+ pos (1- x)))))
			 (unless ok
			   (setf reject-reason 'end-of-input-in-character))
			 ok))
		     (valid-secondary-byte-p (b)
		       (declare (type (unsigned-byte 8) b))
		       (= (logand b #b11000000) #b10000000))
		     (valid-secondary-p (x)
		       (let* ((b (,accessor array (the array-range (+ pos x))))
			      (ok (valid-secondary-byte-p b)))
			 (unless ok
			   (setf reject-reason 'invalid-utf8-continuation-byte)
			   (setf reject-position (+ pos x)))
			 ok))
		     (preliminary-ok-for-length (maybe-len len)
		       (and (eql maybe-len len)
			    (enough-bytes-left-p len)
			    (loop for i from 1 below len
				  always (valid-secondary-p i))))
		     (overlong-chk (x y)
		       (let ((ok (or (/= initial-byte x)
				     (/= (logior (,accessor array (the array-range (+ pos 1)))
						 y)
					 y))))
			 (unless ok
			   (setf reject-reason 'overlong-utf8-sequence))
			 ok))
		     (character-below-char-code-limit-p ()
		       ;; This is only called on a four-byte sequence to
		       ;; ensure we don't go over SBCL's character limts.
		       (let ((ok (cond ((< (aref lexically-max 0) (,accessor array pos))
					nil)
				       ((> (aref lexically-max 0) (,accessor array pos))
					t)
				       ((< (aref lexically-max 1) (,accessor array (+ pos 1)))
					nil)
				       ((> (aref lexically-max 1) (,accessor array (+ pos 1)))
					t)
				       ((< (aref lexically-max 2) (,accessor array (+ pos 2)))
					nil)
				       ((> (aref lexically-max 2) (,accessor array (+ pos 2)))
					t)
				       ((< (aref lexically-max 3) (,accessor array (+ pos 3)))
					nil)
				       (t t))))
			 (unless ok
			   (setf reject-reason 'character-out-of-range))
			 ok)))
	      (declare (inline valid-utf8-starter-byte-p
			       enough-bytes-left-p
			       valid-secondary-byte-p
			       valid-secondary-p
			       preliminary-ok-for-length
			       overlong-chk))
	      (let ((maybe-len (valid-utf8-starter-byte-p initial-byte)))
		(cond ((eql maybe-len 1)
		       (values 1 nil))
		      ((and (preliminary-ok-for-length maybe-len 2)
			    (overlong-chk #b11000000 #b10111111)
			    (overlong-chk #b11000001 #b10111111))
		       (values 2 nil))
		      ((and (preliminary-ok-for-length maybe-len 3)
			    (overlong-chk #b11100000 #b10011111))
		       (values 3 nil))
		      ((and (preliminary-ok-for-length maybe-len 4)
			    (overlong-chk #b11110000 #b10001111)
			    (character-below-char-code-limit-p))
		       (values 4 nil))
		      ((and (preliminary-ok-for-length maybe-len 5)
			    (overlong-chk #b11111000 #b10000111)
			    (not (setf reject-reason 'character-out-of-range)))
		       (bug "can't happen"))
		      ((and (preliminary-ok-for-length maybe-len 6)
			    (overlong-chk #b11111100 #b10000011)
			    (not (setf reject-reason 'character-out-of-range)))
		       (bug "can't happen"))
		      (t
		       (let* ((bad-end (ecase reject-reason
					 (invalid-utf8-starter-byte
					  (1+ pos))
					 (end-of-input-in-character
					  end)
					 (invalid-utf8-continuation-byte
					  reject-position)
					 ((overlong-utf8-sequence character-out-of-range)
					  (+ pos maybe-len))))
			      (bad-len (- bad-end pos)))
			 (declare (type array-range bad-end bad-len))
			 (if replacements-box
			     (values bad-len (pop (cdr replacements-box)))
			     (let ((replacement (decoding-error array pos bad-end :utf-8 reject-reason reject-position)))
			       (values bad-len replacement))))))))))))))
(instantiate-octets-definition define-bytes-per-utf8-character)

(defmacro define-simple-get-utf8-char (accessor type)
  (let ((name (make-od-name 'simple-get-utf8-char accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (array pos bytes)
	(declare (optimize speed (safety 0))
		 (type ,type array)
		 (type array-range pos)
		 (type (integer 1 4) bytes))
	(flet ((cref (x)
		 (,accessor array (the array-range (+ pos x)))))
	  (declare (inline cref))
	  (code-char (ecase bytes
		       (1 (cref 0))
		       (2 (logior (ash (ldb (byte 5 0) (cref 0)) 6)
				  (ldb (byte 6 0) (cref 1))))
		       (3 (logior (ash (ldb (byte 4 0) (cref 0)) 12)
				  (ash (ldb (byte 6 0) (cref 1)) 6)
				  (ldb (byte 6 0) (cref 2))))
		       (4 (logior (ash (ldb (byte 3 0) (cref 0)) 18)
				  (ash (ldb (byte 6 0) (cref 1)) 12)
				  (ash (ldb (byte 6 0) (cref 2)) 6)
				  (ldb (byte 6 0) (cref 3)))))))))))
(instantiate-octets-definition define-simple-get-utf8-char)

(defmacro define-get-utf8-character (accessor type)
  (let ((name (make-od-name 'get-utf8-character accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (array pos end replacements)
	;; Returns the character (or nil) and the number of bytes consumed
	(declare (optimize speed (safety 0))
		 (type ,type array)
		 (type array-range pos end))
	(multiple-value-bind (bytes invalid) (,(make-od-name 'bytes-per-utf8-character accessor) array pos end replacements)
	  (if (not invalid)
	      (values (,(make-od-name 'simple-get-utf8-char accessor) array pos bytes)
		      bytes)
	      (values invalid bytes)))))))
(instantiate-octets-definition define-get-utf8-character)

(defmacro define-utf8->string% (accessor type)
  (let ((name (make-od-name 'utf8->string% accessor)))
    `(progn
      (defun ,name (string sstart send array astart aend replacements)
	(declare (optimize speed (safety 0))
		 (type simple-string string)
		 (type ,type array)
		 (type array-range sstart send astart aend))
	(vari-transcode-aref ; dest is always a string
	  string sstart send
	  array astart aend
	  replacements
	  #',(make-od-name 'get-utf8-character accessor)
	  #'characterp)))))
(instantiate-octets-definition define-utf8->string%)

(defmacro define-utf8->string* (accessor type)
  (let ((name (make-od-name 'utf8->string* accessor)))
    `(progn
      (defun ,name (string sstart send array astart aend)
	(declare (optimize speed (safety 0))
		 (type simple-string string)
		 (type ,type array)
		 (type array-range sstart send astart aend))
	(,(make-od-name 'utf8->string% accessor) string sstart send array astart aend nil)))))
(instantiate-octets-definition define-utf8->string*)

(defmacro define-utf8-string-length (accessor type)
  (let ((name (make-od-name 'utf8-string-length accessor)))
    `(defun ,name (array start end)
      ;; Returns the length and a list of replacements for bad characters
      (declare (optimize speed (safety 0))
	       (type ,type array)
	       (type array-range start end))
      (let* ((bytes 0)
	     (length 0)
	     (replacements-start (cons nil nil))
	     (replacements-end replacements-start))
	(declare (dynamic-extent replacements-start)
		 (type array-range bytes length))
	(flet ((collect (replacement)
		   (setf (cdr replacements-end) (cons replacement nil)
			 replacements-end (cdr replacements-end))
		 replacement))
	  (loop for src = start then (+ src bytes)
		while (< src end)
		do (multiple-value-bind (bytes-this-char invalid) (,(make-od-name 'bytes-per-utf8-character accessor) array src end nil)
		     (declare (type (or null string) invalid))
		     (setf bytes bytes-this-char)
		     (let ((new-length (+ length (if invalid
						     (length (collect invalid))
						     1))))
		       (unless (< new-length #.sb!xc:array-dimension-limit)
			 (error "Replacement string too long"))
		       (setf length new-length)))))
	(values length (cdr replacements-start))))))
(instantiate-octets-definition define-utf8-string-length)

(defmacro define-utf8->string (accessor type)
  (let ((name (make-od-name 'utf8->string accessor)))
    `(progn
      (defun ,name (array astart aend)
	(declare (optimize speed (safety 0))
		 (type ,type array)
		 (type array-range astart aend))
	(let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
	  (loop with pos = astart
		do (multiple-value-bind (bytes invalid) (,(make-od-name 'bytes-per-utf8-character accessor) array pos aend nil)
		     (declare (type (or null string) invalid))
		     (cond
		       ((null invalid)
			(vector-push-extend (,(make-od-name 'simple-get-utf8-char accessor) array pos bytes) string))
		       (t
			(dotimes (i (length invalid))
			  (vector-push-extend (char invalid i) string))))
		     (incf pos bytes))
		while (< pos aend))
	  (coerce string 'simple-string))))))
(instantiate-octets-definition define-utf8->string)

;;;; external formats

(defun default-external-format ()
  (intern (or (sb!alien:alien-funcall
	       (extern-alien "nl_langinfo"
			     (function c-string int))
	       sb!unix:codeset)
	      "LATIN-1")
	  "KEYWORD"))

(defparameter *external-format-functions*
  '(((:ascii :us-ascii :ansi_x3.4-1968)
     ascii->string ascii->string*-aref string->ascii string->ascii*-aref)
    ((:latin1 :latin-1 :iso-8859-1)
     latin1->string latin1->string*-aref string->latin1 string->latin1*-aref)
    #!+sb-unicode
    ((:latin9 :latin-9 :iso-8859-15)
     latin9->string latin9->string*-aref string->latin9 string->latin9*-aref)
    ((:utf8 :utf-8)
     utf8->string-aref utf8->string*-aref string->utf8 string->utf8*-aref)))

(defun external-formats-funs (external-format)
  (when (eql external-format :default)
    (setf external-format (default-external-format)))
  (or (cdr (find external-format (the list *external-format-functions*)
		 :test #'member
		 :key #'car))
      (error "Unknown external-format ~S" external-format)))

;;;; public interface

(defun octets-to-string (vector &key (external-format :default) (start 0) end)
  (declare (type (vector (unsigned-byte 8)) vector))
  (with-array-data ((vector vector)
                    (start start)
                    (end (%check-vector-sequence-bounds vector start end)))
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (funcall (symbol-function (first (external-formats-funs external-format)))
             vector start end)))

(defun octets-to-string* (string vector &key (external-format :default)
                          (start1 0) end1 (start2 0) end2)
  (declare (type string string)
           (type (vector (unsigned-byte 8)) vector))
  (with-array-data
      ((string string)
       (start1 start1)
       (end1 (%check-vector-sequence-bounds string start1 end1)))
    (declare (type simple-string string))
    (with-array-data
        ((vector vector)
         (start2 start2)
         (end2 (%check-vector-sequence-bounds vector start2 end2)))
      (declare (type (simple-array (unsigned-byte 8) (*)) vector))
      (funcall (symbol-function (second (external-formats-funs external-format)))
               string start1 end1 vector start2 end2))))

(defun string-to-octets (string &key (external-format :default)
                         (start 0) end null-terminate)
  (declare (type string string))
  (with-array-data ((string string)
                    (start start)
                    (end (%check-vector-sequence-bounds string start end)))
    (declare (type simple-string string))
    (funcall (symbol-function (third (external-formats-funs external-format)))
             string start end (if null-terminate 1 0))))

(defun string-to-octets* (vector string &key (external-format :default)
                          (start1 0) end1 (start2 0) end2)
  (declare (type (vector (unsigned-byte 8)) vector)
           (type string string))
  (with-array-data
      ((vector vector)
       (start1 start1)
       (end1 (%check-vector-sequence-bounds vector start1 end1)))
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (with-array-data
        ((string string)
         (start2 start2)
         (end2 (%check-vector-sequence-bounds string start2 end2)))
      (declare (type simple-string string))
      (funcall (symbol-function (fourth (external-formats-funs external-format)))
               vector start1 end1 string start2 end2))))

#!+sb-unicode
(defvar +unicode-replacement-character+ (string (code-char #xfffd)))
#!+sb-unicode
(defun use-unicode-replacement-char (condition)
  (use-value +unicode-replacement-character+ condition))

;;; Utilities that maybe should be exported

#!+sb-unicode
(defmacro with-standard-replacement-character (&body body)
  `(handler-bind ((octet-encoding-error #'use-unicode-replacement-char))
    ,@body))

(defmacro with-default-decoding-replacement ((c) &body body)
  (let ((cname (gensym)))
  `(let ((,cname ,c))
    (handler-bind
	((octet-decoding-error (lambda (c)
				 (use-value ,cname c))))
      ,@body))))

;;; debugging stuff
#|
(defmacro show-overflow (&body body)
  `(handler-bind ((octet-buffer-overflow
		   (lambda (c)
		     (format t "Overflowed with ~S~%" (octet-buffer-overflow-replacement c))
		     (finish-output))))
    ,@body))

(defun ub8 (len-or-seq)
  (if (numberp len-or-seq)
      (make-array len-or-seq :element-type '(unsigned-byte 8) :initial-element 0)
      (coerce len-or-seq '(simple-array (unsigned-byte 8) (*)))))

(defun ensure-roundtrip-utf8 ()
  (let ((string (make-string char-code-limit))
	(octets (make-array (* 4 char-code-limit) :element-type '(unsigned-byte 8)))
	(string2 (make-string char-code-limit)))
    (dotimes (i char-code-limit)
      (setf (char string i) (code-char i)))
    (multiple-value-bind (_ octets-length used-chars)
	(string-to-octets* octets string :external-format :utf8)
      (declare (ignore _))
      (assert (= used-chars (length string)))
      (multiple-value-bind (_ string-length used-octets)
	  (octets-to-string* string2 octets :external-format :utf8 :end2 octets-length)
	(declare (ignore _))
	(assert (= used-octets octets-length))
	(assert (= string-length (length string)))
	(assert (string= string string2)))))
  t)

(defun ensure-roundtrip-utf8-2 ()
  (let ((string (make-string char-code-limit)))
    (dotimes (i char-code-limit)
      (setf (char string i) (code-char i)))
    (let ((string2
	   (octets-to-string (string-to-octets string :external-format :utf8)
			   :external-format :utf8)))
      (assert (= (length string2) (length string)))
      (assert (string= string string2))))
  t)

(defun ensure-roundtrip-latin (format)
  (let ((octets (ub8 256))
	(string (make-string 256))
	(octets2 (ub8 256)))
    (dotimes (i 256)
      (setf (aref octets i) i))
    (multiple-value-bind (_ string-length octets-used)
	(octets-to-string* string octets :external-format format)
      (declare (ignore _))
      (assert (= string-length 256))
      (assert (= octets-used 256)))
    (multiple-value-bind (_ octet-length chars-used)
	(string-to-octets* octets2 string :external-format format)
      (declare (ignore _))
      (assert (= octet-length 256))
      (assert (= chars-used 256)))
    (assert (every #'= octets octets2)))
  t)

(defun ensure-roundtrip-latin-2 (format)
  (let ((octets (ub8 256)))
    (dotimes (i 256)
      (setf (aref octets i) i))
    (let* ((str (octets-to-string octets :external-format format))
	   (oct2 (string-to-octets str :external-format format)))
      (assert (= (length octets) (length oct2)))
      (assert (every #'= octets oct2))))
  t)

(defun ensure-roundtrip-latin1 ()
  (ensure-roundtrip-latin :latin1))

(defun ensure-roundtrip-latin9 ()
  (ensure-roundtrip-latin :latin9))

(defun ensure-roundtrip-latin1-2 ()
  (ensure-roundtrip-latin-2 :latin1))

(defun ensure-roundtrip-latin9-2 ()
  (ensure-roundtrip-latin-2 :latin9))

(defmacro i&c (form)
  `(handler-case ,form
    (error (c)
     (format *trace-output* "~S: ~A~%" ',form c))))

(defun test-octets ()
  (i&c (ensure-roundtrip-utf8))
  (i&c (ensure-roundtrip-utf8-2))
  (i&c (ensure-roundtrip-latin1))
  (i&c (ensure-roundtrip-latin1-2))
  (i&c (ensure-roundtrip-latin9))
  (i&c (ensure-roundtrip-latin9-2)))

|#
