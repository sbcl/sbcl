;;;; character functions
;;;;
;;;; This implementation assumes the use of ASCII codes and the
;;;; specific character formats used in SBCL (and its ancestor, CMU
;;;; CL). It is optimized for performance rather than for portability
;;;; and elegance, and may have to be rewritten if the character
;;;; representation is changed.
;;;;
;;;; KLUDGE: As of sbcl-0.6.11.25, at least, the ASCII-dependence is
;;;; not confined to this file. E.g. there are DEFTRANSFORMs in
;;;; srctran.lisp for CHAR-UPCASE, CHAR-EQUAL, and CHAR-DOWNCASE, and
;;;; they assume ASCII. -- WHN 2001-03-25

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; We compile some trivial character operations via inline expansion.
#!-sb-fluid
(declaim (inline standard-char-p graphic-char-p alpha-char-p
		 upper-case-p lower-case-p both-case-p alphanumericp
		 char-int))
(declaim (maybe-inline digit-char-p digit-weight))

(deftype char-code ()
  `(integer 0 (,char-code-limit)))

;;; This is the alist of (character-name . character) for characters
;;; with long names. The first name in this list for a given character
;;; is used on typeout and is the preferred form for input.
(macrolet ((frob (char-names-list)
	     (collect ((results))
	       (dolist (code char-names-list)
		 (destructuring-bind (ccode names) code
		   (dolist (name names)
		     (results (cons name (code-char ccode))))))
	       `(defparameter *char-name-alist* ',(results)))))
  ;; Note: The *** markers here indicate character names which are
  ;; required by the ANSI specification of #'CHAR-NAME. For the others,
  ;; we prefer the ASCII standard name.
  (frob ((#x00 ("Nul" "Null" "^@"))
	 (#x01 ("Soh" "^a"))
	 (#x02 ("Stx" "^b"))
	 (#x03 ("Etx" "^c"))
	 (#x04 ("Eot" "^d"))
	 (#x05 ("Enq" "^e"))
	 (#x06 ("Ack" "^f"))
	 (#x07 ("Bel" "Bell" "^g"))
	 (#x08 ("Backspace" "^h" "Bs")) ; *** See Note above.
	 (#x09 ("Tab" "^i" "Ht")) ; *** See Note above.
	 (#x0A ("Newline" "Linefeed" "^j" "Lf" "Nl" )) ; *** See Note above.
	 (#x0B ("Vt" "^k"))
	 (#x0C ("Page" "^l" "Form" "Formfeed" "Ff" "Np")) ; *** See Note above.
	 (#x0D ("Return" "^m" "Cr")) ; *** See Note above.
	 (#x0E ("So" "^n"))
	 (#x0F ("Si" "^o"))
	 (#x10 ("Dle" "^p"))
	 (#x11 ("Dc1" "^q"))
	 (#x12 ("Dc2" "^r"))
	 (#x13 ("Dc3" "^s"))
	 (#x14 ("Dc4" "^t"))
	 (#x15 ("Nak" "^u"))
	 (#x16 ("Syn" "^v"))
	 (#x17 ("Etb" "^w"))
	 (#x18 ("Can" "^x"))
	 (#x19 ("Em" "^y"))
	 (#x1A ("Sub" "^z"))
	 (#x1B ("Esc" "Escape" "^[" "Altmode" "Alt"))
	 (#x1C ("Fs" "^\\"))
	 (#x1D ("Gs" "^]"))
	 (#x1E ("Rs" "^^"))
	 (#x1F ("Us" "^_"))
	 (#x20 ("Space" "Sp")) ; *** See Note above.
	 (#x7f ("Rubout" "Delete" "Del"))))) ; *** See Note above.

;;;; accessor functions

(defun char-code (char)
  #!+sb-doc
  "Return the integer code of CHAR."
  (etypecase char
    (base-char (char-code (truly-the base-char char)))))

(defun char-int (char)
  #!+sb-doc
  "Return the integer code of CHAR. (In SBCL this is the same as CHAR-CODE, as
   there are no character bits or fonts.)"
  (char-code char))

(defun code-char (code)
  #!+sb-doc
  "Return the character with the code CODE."
  (declare (type char-code code))
  (code-char code))

(defun character (object)
  #!+sb-doc
  "Coerce OBJECT into a CHARACTER if possible. Legal inputs are 
  characters, strings and symbols of length 1."
  (flet ((do-error (control args)
	   (error 'simple-type-error
		  :datum object
		  ;;?? how to express "symbol with name of length 1"?
		  :expected-type '(or character (string 1))
		  :format-control control
		  :format-arguments args)))
    (typecase object
      (character object)
      (string (if (= 1 (length (the string object)))
		  (char object 0)
		  (do-error
		   "String is not of length one: ~S" (list object))))
      (symbol (if (= 1 (length (symbol-name object)))
		  (schar (symbol-name object) 0)
		  (do-error
		   "Symbol name is not of length one: ~S" (list object))))
      (t (do-error "~S cannot be coerced to a character." (list object))))))

(defun char-name (char)
  #!+sb-doc
  "Return the name (a STRING) for a CHARACTER object."
  (car (rassoc char *char-name-alist*)))

(defun name-char (name)
  #!+sb-doc
  "Given an argument acceptable to STRING, NAME-CHAR returns a character
  whose name is that string, if one exists. Otherwise, NIL is returned."
  (cdr (assoc (string name) *char-name-alist* :test #'string-equal)))

;;;; predicates

(defun standard-char-p (char)
  #!+sb-doc
  "The argument must be a character object. Standard-char-p returns T if the
   argument is a standard character -- one of the 95 ASCII printing characters
   or <return>."
  (declare (character char))
  (and (typep char 'base-char)
       (let ((n (char-code (the base-char char))))
	 (or (< 31 n 127)
	     (= n 10)))))

(defun %standard-char-p (thing)
  #!+sb-doc
  "Return T if and only if THING is a standard-char. Differs from
  standard-char-p in that THING doesn't have to be a character."
  (and (characterp thing) (standard-char-p thing)))

(defun graphic-char-p (char)
  #!+sb-doc
  "The argument must be a character object. Graphic-char-p returns T if the
  argument is a printing character (space through ~ in ASCII), otherwise
  returns ()."
  (declare (character char))
  (and (typep char 'base-char)
       (< 31
	  (char-code (the base-char char))
	  127)))

(defun alpha-char-p (char)
  #!+sb-doc
  "The argument must be a character object. Alpha-char-p returns T if the
   argument is an alphabetic character, A-Z or a-z; otherwise ()."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123))))

(defun upper-case-p (char)
  #!+sb-doc
  "The argument must be a character object; upper-case-p returns T if the
   argument is an upper-case character, () otherwise."
  (declare (character char))
  (< 64
     (char-code char)
     91))

(defun lower-case-p (char)
  #!+sb-doc
  "The argument must be a character object; lower-case-p returns T if the
   argument is a lower-case character, () otherwise."
  (declare (character char))
  (< 96
     (char-code char)
     123))

(defun both-case-p (char)
  #!+sb-doc
  "The argument must be a character object. Both-case-p returns T if the
  argument is an alphabetic character and if the character exists in
  both upper and lower case. For ASCII, this is the same as Alpha-char-p."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 64 m 91) (< 96 m 123))))

(defun digit-char-p (char &optional (radix 10.))
  #!+sb-doc
  "If char is a digit in the specified radix, returns the fixnum for
  which that digit stands, else returns NIL. Radix defaults to 10
  (decimal)."
  (declare (character char) (type (integer 2 36) radix))
  (let ((m (- (char-code char) 48)))
    (declare (fixnum m))
    (cond ((<= radix 10.)
	   ;; Special-case decimal and smaller radices.
	   (if (and (>= m 0) (< m radix))  m  nil))
	  ;; Digits 0 - 9 are used as is, since radix is larger.
	  ((and (>= m 0) (< m 10)) m)
	  ;; Check for upper case A - Z.
	  ((and (>= (setq m (- m 7)) 10) (< m radix)) m)
	  ;; Also check lower case a - z.
	  ((and (>= (setq m (- m 32)) 10) (< m radix)) m)
	  ;; Else, fail.
	  (t nil))))

(defun whitespace-char-p (x)
  (and (characterp x)
       (or (char= x #\space)
	   (char= x (code-char tab-char-code))
	   (char= x (code-char return-char-code))
	   (char= x #\linefeed))))

(defun alphanumericp (char)
  #!+sb-doc
  "Given a character-object argument, alphanumericp returns T if the
   argument is either numeric or alphabetic."
  (declare (character char))
  (let ((m (char-code char)))
    (or (< 47 m 58) (< 64 m 91) (< 96 m 123))))

(defun char= (character &rest more-characters)
  #!+sb-doc
  "Return T if all of the arguments are the same character."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (eq (car clist) character) (return nil))))

(defun char/= (character &rest more-characters)
  #!+sb-doc
  "Return T if no two of the arguments are the same character."
  (do* ((head character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (do* ((l list (cdr l)))		  ;inner loop returns T
		 ((atom l) T)			     ; iff head /= rest.
	      (if (eq head (car l)) (return nil)))
      (return nil))))

(defun char< (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly increasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (< (char-int c)
	       (char-int (car list)))
      (return nil))))

(defun char> (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly decreasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (> (char-int c)
	       (char-int (car list)))
      (return nil))))

(defun char<= (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly non-decreasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (<= (char-int c)
		(char-int (car list)))
      (return nil))))

(defun char>= (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly non-increasing alphabetic order."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (>= (char-int c)
		(char-int (car list)))
      (return nil))))

;;; Equal-Char-Code is used by the following functions as a version of char-int
;;;  which loses font, bits, and case info.

(defmacro equal-char-code (character)
  `(let ((ch (char-code ,character)))
     (if (< 96 ch 123) (- ch 32) ch)))

(defun char-equal (character &rest more-characters)
  #!+sb-doc
  "Return T if all of the arguments are the same character.
  Font, bits, and case are ignored."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (= (equal-char-code (car clist))
	       (equal-char-code character))
      (return nil))))

(defun char-not-equal (character &rest more-characters)
  #!+sb-doc
  "Return T if no two of the arguments are the same character.
   Font, bits, and case are ignored."
  (do* ((head character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (do* ((l list (cdr l)))
		 ((atom l) T)
	      (if (= (equal-char-code head)
		     (equal-char-code (car l)))
		  (return nil)))
      (return nil))))

(defun char-lessp (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly increasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (< (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))

(defun char-greaterp (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly decreasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (> (equal-char-code c)
	       (equal-char-code (car list)))
      (return nil))))

(defun char-not-greaterp (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly non-decreasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (<= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))

(defun char-not-lessp (character &rest more-characters)
  #!+sb-doc
  "Return T if the arguments are in strictly non-increasing alphabetic order.
   Font, bits, and case are ignored."
  (do* ((c character (car list))
	(list more-characters (cdr list)))
       ((atom list) T)
    (unless (>= (equal-char-code c)
		(equal-char-code (car list)))
      (return nil))))

;;;; miscellaneous functions

(defun char-upcase (char)
  #!+sb-doc
  "Return CHAR converted to upper-case if that is possible."
  (declare (character char))
  (if (lower-case-p char)
      (code-char (- (char-code char) 32))
      char))

(defun char-downcase (char)
  #!+sb-doc
  "Return CHAR converted to lower-case if that is possible."
  (declare (character char))
  (if (upper-case-p char)
      (code-char (+ (char-code char) 32))
      char))

(defun digit-char (weight &optional (radix 10))
  #!+sb-doc
  "All arguments must be integers. Returns a character object that
  represents a digit of the given weight in the specified radix. Returns
  NIL if no such character exists. The character will have the specified
  font attributes."
  (declare (type (integer 2 36) radix) (type unsigned-byte weight))
  (and (typep weight 'fixnum)
       (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))
