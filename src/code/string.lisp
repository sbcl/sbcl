;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun string (x)
  #!+sb-doc
  "Coerces X into a string. If X is a string, X is returned. If X is a
   symbol, X's pname is returned. If X is a character then a one element
   string containing that character is returned. If X cannot be coerced
   into a string, an error occurs."
  (cond ((stringp x) x)
	((symbolp x) (symbol-name x))
	((characterp x)
	 (let ((res (make-string 1)))
	   (setf (schar res 0) x) res))
	(t
	 (error 'simple-type-error
		:datum x
		:expected-type 'stringable
		:format-control "~S cannot be coerced to a string."
		:format-arguments (list x)))))

;;; With-One-String is used to set up some string hacking things. The keywords
;;; are parsed, and the string is hacked into a simple-string.

(eval-when (:compile-toplevel)

(sb!xc:defmacro with-one-string (string start end cum-offset &rest forms)
  `(let ((,string (if (stringp ,string) ,string (string ,string))))
     (with-array-data ((,string ,string :offset-var ,cum-offset)
		       (,start ,start)
		       (,end (or ,end (length (the vector ,string)))))
       ,@forms)))

) ; EVAN-WHEN

;;; With-String is like With-One-String, but doesn't parse keywords.

(eval-when (:compile-toplevel)

(sb!xc:defmacro with-string (string &rest forms)
  `(let ((,string (if (stringp ,string) ,string (string ,string))))
     (with-array-data ((,string ,string)
		       (start)
		       (end (length (the vector ,string))))
       ,@forms)))

) ; EVAL-WHEN

;;; With-Two-Strings is used to set up string comparison operations. The
;;; keywords are parsed, and the strings are hacked into simple-strings.

(eval-when (:compile-toplevel)

(sb!xc:defmacro with-two-strings (string1 string2 start1 end1 cum-offset-1
					    start2 end2 &rest forms)
  `(let ((,string1 (if (stringp ,string1) ,string1 (string ,string1)))
	 (,string2 (if (stringp ,string2) ,string2 (string ,string2))))
     (with-array-data ((,string1 ,string1 :offset-var ,cum-offset-1)
		       (,start1 ,start1)
		       (,end1 (or ,end1 (length (the vector ,string1)))))
       (with-array-data ((,string2 ,string2)
			 (,start2 ,start2)
			 (,end2 (or ,end2 (length (the vector ,string2)))))
	 ,@forms))))

) ; EVAL-WHEN

(defun char (string index)
  #!+sb-doc
  "Given a string and a non-negative integer index less than the length of
  the string, returns the character object representing the character at
  that position in the string."
  (declare (optimize (safety 1)))
  (char string index))

(defun %charset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (char string index) new-el))

(defun schar (string index)
  #!+sb-doc
  "SCHAR returns the character object at an indexed position in a string
   just as CHAR does, except the string must be a simple-string."
  (declare (optimize (safety 1)))
  (schar string index))

(defun %scharset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (schar string index) new-el))

(defun string=* (string1 string2 start1 end1 start2 end2)
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (not (%sp-string-compare string1 start1 end1 string2 start2 end2))))

(defun string/=* (string1 string2 start1 end1 start2 end2)
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((comparison (%sp-string-compare string1 start1 end1
					  string2 start2 end2)))
      (if comparison (- (the fixnum comparison) offset1)))))

(eval-when (:compile-toplevel :execute)

;;; Lessp is true if the desired expansion is for string<* or string<=*.
;;; Equalp is true if the desired expansion is for string<=* or string>=*.
(sb!xc:defmacro string<>=*-body (lessp equalp)
  (let ((offset1 (gensym)))
    `(with-two-strings string1 string2 start1 end1 ,offset1 start2 end2
       (let ((index (%sp-string-compare string1 start1 end1
					string2 start2 end2)))
	 (if index
	     (cond ((= (the fixnum index) (the fixnum end1))
		    ,(if lessp
			 `(- (the fixnum index) ,offset1)
		       `nil))
		   ((= (+ (the fixnum index) (- start2 start1))
		       (the fixnum end2))
		    ,(if lessp
			 `nil
		       `(- (the fixnum index) ,offset1)))
		   ((,(if lessp 'char< 'char>)
		     (schar string1 index)
		     (schar string2 (+ (the fixnum index) (- start2 start1))))
		    (- (the fixnum index) ,offset1))
		   (t nil))
	     ,(if equalp `(- (the fixnum end1) ,offset1) 'nil))))))
) ; eval-when

(defun string<* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body t nil))

(defun string>* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body nil nil))

(defun string<=* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body t t))

(defun string>=* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body nil t))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string<* string1 string2 start1 end1 start2 end2))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string>* string1 string2 start1 end1 start2 end2))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string<=* string1 string2 start1 end1 start2 end2))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string>=* string1 string2 start1 end1 start2 end2))

;;; Note: (STRING= "PREFIX" "SHORT" :END2 (LENGTH "PREFIX")) gives
;;; an error instead of returning NIL as I would have expected.
;;; The ANSI spec for STRING= itself doesn't seem to clarify this
;;; much, but the SUBSEQ-OUT-OF-BOUNDS writeup seems to say that
;;; this is conforming (and required) behavior, because any index
;;; out of range is an error. (So there seems to be no concise and
;;; efficient way to test for strings which begin with a particular
;;; pattern. Alas..) -- WHN 19991206
(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char=)."
  (string=* string1 string2 start1 end1 start2 end2))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string/=* string1 string2 start1 end1 start2 end2))

(eval-when (:compile-toplevel :execute)

;;; STRING-NOT-EQUAL-LOOP is used to generate character comparison loops for
;;; STRING-EQUAL and STRING-NOT-EQUAL.
(sb!xc:defmacro string-not-equal-loop (end
					 end-value
					 &optional (abort-value nil abortp))
  (declare (fixnum end))
  (let ((end-test (if (= end 1)
		      `(= index1 (the fixnum end1))
		      `(= index2 (the fixnum end2)))))
    `(do ((index1 start1 (1+ index1))
	  (index2 start2 (1+ index2)))
	 (,(if abortp
	       end-test
	       `(or ,end-test
		    (not (char-equal (schar string1 index1)
				     (schar string2 index2)))))
	  ,end-value)
       (declare (fixnum index1 index2))
       ,@(if abortp
	     `((if (not (char-equal (schar string1 index1)
				    (schar string2 index2)))
		   (return ,abort-value)))))))

) ; EVAL-WHEN

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char-equal)."
  (declare (fixnum start1 start2))
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((slen1 (- (the fixnum end1) start1))
	  (slen2 (- (the fixnum end2) start2)))
      (declare (fixnum slen1 slen2))
      (if (or (minusp slen1) (minusp slen2))
	  ;;prevent endless looping later.
	  (error "Improper bounds for string comparison."))
      (if (= slen1 slen2)
	  ;;return () immediately if lengths aren't equal.
	  (string-not-equal-loop 1 t nil)))))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((slen1 (- end1 start1))
	  (slen2 (- end2 start2)))
      (declare (fixnum slen1 slen2))
      (if (or (minusp slen1) (minusp slen2))
	  ;;prevent endless looping later.
	  (error "Improper bounds for string comparison."))
      (cond ((or (minusp slen1) (or (minusp slen2)))
	     (error "Improper substring for comparison."))
	    ((= slen1 slen2)
	     (string-not-equal-loop 1 nil (- index1 offset1)))
	    ((< slen1 slen2)
	     (string-not-equal-loop 1 (- index1 offset1)))
	    (t
	     (string-not-equal-loop 2 (- index1 offset1)))))))

(eval-when (:compile-toplevel :execute)

;;; STRING-LESS-GREATER-EQUAL-TESTS returns a test on the lengths of string1
;;; and string2 and a test on the current characters from string1 and string2
;;; for the following macro.
(defun string-less-greater-equal-tests (lessp equalp)
  (if lessp
      (if equalp
	  ;; STRING-NOT-GREATERP
	  (values '<= `(not (char-greaterp char1 char2)))
	  ;; STRING-LESSP
	  (values '< `(char-lessp char1 char2)))
      (if equalp
	  ;; STRING-NOT-LESSP
	  (values '>= `(not (char-lessp char1 char2)))
	  ;; STRING-GREATERP
	  (values '> `(char-greaterp char1 char2)))))

(sb!xc:defmacro string-less-greater-equal (lessp equalp)
  (multiple-value-bind (length-test character-test)
      (string-less-greater-equal-tests lessp equalp)
    `(with-two-strings string1 string2 start1 end1 offset1 start2 end2
       (let ((slen1 (- (the fixnum end1) start1))
	     (slen2 (- (the fixnum end2) start2)))
	 (declare (fixnum slen1 slen2))
	 (if (or (minusp slen1) (minusp slen2))
	     ;;prevent endless looping later.
	     (error "Improper bounds for string comparison."))
	 (do ((index1 start1 (1+ index1))
	      (index2 start2 (1+ index2))
	      (char1)
	      (char2))
	     ((or (= index1 (the fixnum end1)) (= index2 (the fixnum end2)))
	      (if (,length-test slen1 slen2) (- index1 offset1)))
	   (declare (fixnum index1 index2))
	   (setq char1 (schar string1 index1))
	   (setq char2 (schar string2 index2))
	   (if (not (char-equal char1 char2))
	       (if ,character-test
		   (return (- index1 offset1))
		   (return ()))))))))

) ; EVAL-WHEN

(defun string-lessp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal t nil))

(defun string-greaterp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal nil nil))

(defun string-not-lessp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal nil t))

(defun string-not-greaterp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal t t))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-lessp* string1 string2 start1 end1 start2 end2))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-greaterp* string1 string2 start1 end1 start2 end2))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-lessp* string1 string2 start1 end1 start2 end2))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0)
				    end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-greaterp* string1 string2 start1 end1 start2 end2))

(defun make-string (count &key element-type ((:initial-element fill-char)))
  #!+sb-doc
  "Given a character count and an optional fill character, makes and returns
   a new string Count long filled with the fill character."
  (declare (fixnum count)
	   (ignore element-type))
  (if fill-char
      (do ((i 0 (1+ i))
	   (string (make-string count)))
	  ((= i count) string)
	(declare (fixnum i))
	(setf (schar string i) fill-char))
      (make-string count)))

(defun string-upcase (string &key (start 0) end)
  #!+sb-doc
  "Given a string, returns a new string that is a copy of it with
  all lower case alphabetic characters converted to uppercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index)))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index)
		(char-upcase (schar string index))))
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))

(defun string-downcase (string &key (start 0) end)
  #!+sb-doc
  "Given a string, returns a new string that is a copy of it with
  all upper case alphabetic characters converted to lowercase."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index)))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index)
		(char-downcase (schar string index))))
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))

(defun string-capitalize (string &key (start 0) end)
  #!+sb-doc
  "Given a string, returns a copy of the string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
  (declare (fixnum start))
  (let* ((string (if (stringp string) string (string string)))
	 (slen (length string)))
    (declare (fixnum slen))
    (with-one-string string start end offset
      (let ((offset-slen (+ slen offset))
	    (newstring (make-string slen)))
	(declare (fixnum offset-slen))
	(do ((index offset (1+ index))
	     (new-index 0 (1+ new-index)))
	    ((= index start))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	(do ((index start (1+ index))
	     (new-index (- start offset) (1+ new-index))
	     (newword t)
	     (char ()))
	    ((= index (the fixnum end)))
	  (declare (fixnum index new-index))
	  (setq char (schar string index))
	  (cond ((not (alphanumericp char))
		 (setq newword t))
		(newword
		 ;;char is first case-modifiable after non-case-modifiable
		 (setq char (char-upcase char))
		 (setq newword ()))
		;;char is case-modifiable, but not first
		(t (setq char (char-downcase char))))
	  (setf (schar newstring new-index) char))
	(do ((index end (1+ index))
	     (new-index (- (the fixnum end) offset) (1+ new-index)))
	    ((= index offset-slen))
	  (declare (fixnum index new-index))
	  (setf (schar newstring new-index) (schar string index)))
	newstring))))

(defun nstring-upcase (string &key (start 0) end)
  #!+sb-doc
  "Given a string, returns that string with all lower case alphabetic
  characters converted to uppercase."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setf (schar string index) (char-upcase (schar string index)))))
    save-header))

(defun nstring-downcase (string &key (start 0) end)
  #!+sb-doc
  "Given a string, returns that string with all upper case alphabetic
  characters converted to lowercase."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index)))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setf (schar string index) (char-downcase (schar string index)))))
    save-header))

(defun nstring-capitalize (string &key (start 0) end)
  #!+sb-doc
  "Given a string, returns that string with the first
  character of each ``word'' converted to upper-case, and remaining
  chars in the word converted to lower case. A ``word'' is defined
  to be a string of case-modifiable characters delimited by
  non-case-modifiable chars."
  (declare (fixnum start))
  (let ((save-header string))
    (with-one-string string start end offset
      (do ((index start (1+ index))
	   (newword t)
	   (char ()))
	  ((= index (the fixnum end)))
	(declare (fixnum index))
	(setq char (schar string index))
	(cond ((not (alphanumericp char))
	       (setq newword t))
	      (newword
	       ;;char is first case-modifiable after non-case-modifiable
	       (setf (schar string index) (char-upcase char))
	       (setq newword ()))
	      (t
	       (setf (schar string index) (char-downcase char))))))
    save-header))

(defun string-left-trim (char-bag string)
  #!+sb-doc
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  left end."
  (with-string string
    (do ((index start (1+ index)))
	((or (= index (the fixnum end))
	     (not (find (schar string index) char-bag :test #'char=)))
	 (subseq (the simple-string string) index end))
      (declare (fixnum index)))))

(defun string-right-trim (char-bag string)
  #!+sb-doc
  "Given a set of characters (a list or string) and a string, returns
  a copy of the string with the characters in the set removed from the
  right end."
  (with-string string
    (do ((index (1- (the fixnum end)) (1- index)))
	((or (< index start)
	     (not (find (schar string index) char-bag :test #'char=)))
	 (subseq (the simple-string string) start (1+ index)))
      (declare (fixnum index)))))

(defun string-trim (char-bag string)
  #!+sb-doc
  "Given a set of characters (a list or string) and a string, returns a
  copy of the string with the characters in the set removed from both
  ends."
  (with-string string
    (let* ((left-end (do ((index start (1+ index)))
			 ((or (= index (the fixnum end))
			      (not (find (schar string index)
					 char-bag
					 :test #'char=)))
			  index)
		       (declare (fixnum index))))
	   (right-end (do ((index (1- (the fixnum end)) (1- index)))
			  ((or (< index left-end)
			       (not (find (schar string index)
					  char-bag
					  :test #'char=)))
			   (1+ index))
			(declare (fixnum index)))))
      (subseq (the simple-string string) left-end right-end))))
