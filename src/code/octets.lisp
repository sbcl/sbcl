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

;;;; to-octets conversions

;;; to latin (including ascii)

(defmacro define-unibyte-mapper (byte-char-name code-byte-name &rest exceptions)
  `(progn
    (declaim (inline ,byte-char-name ,code-byte-name))
    (defun ,byte-char-name (byte)
      (declare (optimize speed (safety 0))
	       (type (unsigned-byte 8) byte))
      (aref ,(make-array 256
			 :initial-contents (loop for byte below 256
						 collect
						  (let ((exception (cadr (assoc byte exceptions))))
						    (if exception
							exception
							byte))))
	    byte))
    (defun ,code-byte-name (code)
      (declare (optimize speed (safety 0))
	       (type char-code code))
      (case code
	(,(mapcar #'car exceptions) nil)
	,@(mapcar (lambda (exception)
		    (destructuring-bind (byte code) exception
		      `(,code ,byte)))
		  exceptions)
	(otherwise code)))))

#!+sb-unicode
(define-unibyte-mapper
    latin9->code-mapper
    code->latin9-mapper
  (#xA4 #x20AC)
  (#xA6 #x0160)
  (#xA8 #x0161)
  (#xB4 #x017D)
  (#xB8 #x017E)
  (#xBC #x0152)
  (#xBD #x0153)
  (#xBE #x0178))

(declaim (inline get-latin-bytes))
(defun get-latin-bytes (mapper external-format string pos end)
  (declare (ignore end))
  (let ((code (funcall mapper (char-code (char string pos)))))
    (values (cond
	      ((and code (< code 256)) code)
	      (t
	       (encoding-error external-format string pos)))
	    1)))

(declaim (inline code->ascii-mapper))
(defun code->ascii-mapper (code)
  (declare (optimize speed (safety 0))
	   (type char-code code))
  (if (> code 127)
      nil
      code))

(declaim (inline get-ascii-bytes))
(defun get-ascii-bytes (string pos end)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range pos end))
  (get-latin-bytes #'code->ascii-mapper :ascii string pos end))

(declaim (inline get-latin1-bytes))
(defun get-latin1-bytes (string pos end)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range pos end))
  (get-latin-bytes #'identity :latin-1 string pos end))

#!+sb-unicode
(progn
  (declaim (inline get-latin9-bytes))
  (defun get-latin9-bytes (string pos end)
    (declare (optimize speed (safety 0))
             (type simple-string string)
             (type array-range pos end))
    (get-latin-bytes #'code->latin9-mapper :latin-9 string pos end)))

(declaim (inline string->latin%))
(defun string->latin% (string sstart send get-bytes null-padding)
  (declare (optimize speed)
	   (type simple-string string)
	   (type array-range sstart send null-padding)
	   (type function get-bytes))
  (let ((octets (make-array 0 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (loop for pos from sstart below send
	  do (let ((byte-or-bytes (funcall get-bytes string pos send)))
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
(defun char-len-as-utf8 (code)
  (declare (optimize speed (safety 0))
	   (type (integer 0 (#.sb!xc:char-code-limit)) code))
  (cond ((< code 0) (bug "can't happen"))
	((< code #x80) 1)
	((< code #x800) 2)
	((< code #x10000) 3)
	((< code #x110000) 4)
	(t (bug "can't happen"))))

(declaim (inline char->utf8))
(defun char->utf8 (char dest)
  (declare (optimize speed (safety 0))
	   (type (array (unsigned-byte 8) (*)) dest))
  (let ((code (char-code char)))
    (flet ((add-byte (b)
	     (declare (type (unsigned-byte 8) b))
	     (vector-push-extend b dest)))
      (declare (inline add-byte))
      (ecase (char-len-as-utf8 code)
	(1
	 (add-byte code))
	(2
	 (add-byte (logior #b11000000 (ldb (byte 5 6) code)))
	 (add-byte (logior #b10000000 (ldb (byte 6 0) code))))
	(3
	 (add-byte (logior #b11100000 (ldb (byte 4 12) code)))
	 (add-byte (logior #b10000000 (ldb (byte 6 6) code)))
	 (add-byte (logior #b10000000 (ldb (byte 6 0) code))))
	(4
	 (add-byte (logior #b11110000 (ldb (byte 3 18) code)))
	 (add-byte (logior #b10000000 (ldb (byte 6 12) code)))
	 (add-byte (logior #b10000000 (ldb (byte 6 6) code)))
	 (add-byte (logior #b10000000 (ldb (byte 6 0) code))))))))

(defun string->utf8 (string sstart send additional-space)
  (declare (optimize speed (safety 0))
	   (type simple-string string)
	   (type array-range sstart send additional-space))
  (let ((array (make-array (+ additional-space (- send sstart))
			   :element-type '(unsigned-byte 8)
			   :adjustable t
			   :fill-pointer 0)))
    (loop for i from sstart below send
	  do (char->utf8 (char string i) array))
    (dotimes (i additional-space)
      (vector-push-extend 0 array))
    (coerce array '(simple-array (unsigned-byte 8) (*)))))

;;;; to-string conversions

;;; from latin (including ascii)

(defmacro define-ascii->string (accessor type)
  (let ((name (make-od-name 'ascii->string accessor)))
    `(progn
      (defun ,name (array astart aend)
	(declare (optimize speed)
		 (type ,type array)
		 (type array-range astart aend))
	;; Since there is such a thing as a malformed ascii byte, a
	;; simple "make the string, fill it in" won't do.
	(let ((string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
	  (loop for apos from astart below aend
		do (let* ((code (,accessor array apos))
			  (string-content
			   (if (< code 128)
			       (code-char code)
			       (decoding-error array apos (1+ apos) :ascii
					       'malformed-ascii apos))))
		     (if (characterp string-content)
			 (vector-push-extend string-content string)
			 (loop for c across string-content
			       do (vector-push-extend c string))))
		finally (return (coerce string 'simple-string))))))))
(instantiate-octets-definition define-ascii->string)

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

(defmacro define-latin->string (accessor type)
  (let ((name (make-od-name 'latin->string accessor)))
    `(progn
      (declaim (inline latin->string))
      (defun ,name (array astart aend mapper)
	(declare (optimize speed (safety 0))
		 (type ,type array)
		 (type array-range astart aend)
		 (type function mapper))
	(let ((length (the array-range (- aend astart))))
	  (values (,(make-od-name 'latin->string* accessor) (make-string length) 0 length
		                                            array astart aend
		                                            mapper)))))))
(instantiate-octets-definition define-latin->string)

(defmacro define-latin1->string (accessor type)
  (declare (ignore type))
  `(defun ,(make-od-name 'latin1->string accessor) (array astart aend)
    (,(make-od-name 'latin->string accessor) array astart aend #'identity)))
(instantiate-octets-definition define-latin1->string)

#!+sb-unicode
(progn
  (defmacro define-latin9->string (accessor type)
    (declare (ignore type))
    `(defun ,(make-od-name 'latin9->string accessor) (array astart aend)
      (,(make-od-name 'latin->string accessor) array astart aend #'latin9->code-mapper)))
  (instantiate-octets-definition define-latin9->string))

;;; from utf8

(defmacro define-bytes-per-utf8-character (accessor type)
  (let ((name (make-od-name 'bytes-per-utf8-character accessor)))
    `(progn
      ;;(declaim (inline ,name))
      (let ((lexically-max
             (string->utf8 (string (code-char ,(1- sb!xc:char-code-limit)))
                           0 1 0)))
	(declare (type (simple-array (unsigned-byte 8) (#!+sb-unicode 4 #!-sb-unicode 2)) lexically-max))
	(defun ,name (array pos end)
	  (declare (optimize speed (safety 0))
		   (type ,type array)
		   (type array-range pos end))
	  ;; returns the number of bytes consumed and nil if it's a
	  ;; valid character or the number of bytes consumed and a
	  ;; replacement string if it's not.
	  (let ((initial-byte (,accessor array pos))
		(reject-reason nil)
		(reject-position pos)
		(remaining-bytes (- end pos)))
	    (declare (type array-range reject-position remaining-bytes))
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
		     (valid-secondary-p (x)
		       (let* ((idx (the array-range (+ pos x)))
			      (b (,accessor array idx))
			      (ok (= (logand b #b11000000) #b10000000)))
			 (unless ok
			   (setf reject-reason 'invalid-utf8-continuation-byte)
			   (setf reject-position idx))
			 ok))
		     (preliminary-ok-for-length (maybe-len len)
		       (and (eql maybe-len len)
			    ;; Has to be done in this order so that
			    ;; certain broken sequences (e.g., the
			    ;; two-byte sequence `"initial (length 3)"
			    ;; "non-continuation"' -- `#xef #x32')
			    ;; signal only part of that sequence as
			    ;; erronous.
			    (loop for i from 1 below (min len remaining-bytes)
				  always (valid-secondary-p i))
			    (enough-bytes-left-p len)))
		     (overlong-chk (x y)
		       (let ((ok (or (/= initial-byte x)
				     (/= (logior (,accessor array (the array-range (+ pos 1)))
						 y)
					 y))))
			 (unless ok
			   (setf reject-reason 'overlong-utf8-sequence))
			 ok))
		     (character-below-char-code-limit-p ()
		       ;; This is only called on a four-byte sequence
		       ;; (two in non-unicode builds) to ensure we
		       ;; don't go over SBCL's character limts.
		       (let ((ok (cond ((< (aref lexically-max 0) (,accessor array pos))
					nil)
				       ((> (aref lexically-max 0) (,accessor array pos))
					t)
				       ((< (aref lexically-max 1) (,accessor array (+ pos 1)))
					nil)
				       #!+sb-unicode
				       ((> (aref lexically-max 1) (,accessor array (+ pos 1)))
					t)
				       #!+sb-unicode
				       ((< (aref lexically-max 2) (,accessor array (+ pos 2)))
					nil)
				       #!+sb-unicode
				       ((> (aref lexically-max 2) (,accessor array (+ pos 2)))
					t)
				       #!+sb-unicode
				       ((< (aref lexically-max 3) (,accessor array (+ pos 3)))
					nil)
				       (t t))))
			 (unless ok
			   (setf reject-reason 'character-out-of-range))
			 ok)))
	      (declare (inline valid-utf8-starter-byte-p
			       enough-bytes-left-p
			       valid-secondary-p
			       preliminary-ok-for-length
			       overlong-chk))
	      (let ((maybe-len (valid-utf8-starter-byte-p initial-byte)))
		(cond ((eql maybe-len 1)
		       (values 1 nil))
		      ((and (preliminary-ok-for-length maybe-len 2)
			    (overlong-chk #b11000000 #b10111111)
			    (overlong-chk #b11000001 #b10111111)
			    #!-sb-unicode (character-below-char-code-limit-p))
		       (values 2 nil))
		      ((and (preliminary-ok-for-length maybe-len 3)
			    (overlong-chk #b11100000 #b10011111)
			    #!-sb-unicode (not (setf reject-reason 'character-out-of-range)))
		       (values 3 nil))
		      ((and (preliminary-ok-for-length maybe-len 4)
			    (overlong-chk #b11110000 #b10001111)
			    #!-sb-unicode (not (setf reject-reason 'character-out-of-range))
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
			 (let ((replacement (decoding-error array pos bad-end :utf-8 reject-reason reject-position)))
			   (values bad-len replacement)))))))))))))
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

(defmacro define-utf8->string (accessor type)
  (let ((name (make-od-name 'utf8->string accessor)))
    `(progn
      (defun ,name (array astart aend)
	(declare (optimize speed (safety 0))
		 (type ,type array)
		 (type array-range astart aend))
	(let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
	  (loop with pos = astart
		do (multiple-value-bind (bytes invalid)
		       (,(make-od-name 'bytes-per-utf8-character accessor) array pos aend)
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
     ascii->string-aref string->ascii)
    ((:latin1 :latin-1 :iso-8859-1)
     latin1->string-aref string->latin1)
    #!+sb-unicode
    ((:latin9 :latin-9 :iso-8859-15)
     latin9->string-aref string->latin9)
    ((:utf8 :utf-8)
     utf8->string-aref string->utf8)))

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

(defun string-to-octets (string &key (external-format :default)
                         (start 0) end null-terminate)
  (declare (type string string))
  (with-array-data ((string string)
                    (start start)
                    (end (%check-vector-sequence-bounds string start end)))
    (declare (type simple-string string))
    (funcall (symbol-function (second (external-formats-funs external-format)))
             string start end (if null-terminate 1 0))))

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
