;;;; file system interface functions -- fairly Unix-specific

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; Unix pathname host support

;;; Unix namestrings have the following format:
;;;
;;; namestring := [ directory ] [ file [ type [ version ]]]
;;; directory := [ "/" ] { file "/" }*
;;; file := [^/]*
;;; type := "." [^/.]*
;;; version := "." ([0-9]+ | "*")
;;;
;;; Note: this grammar is ambiguous. The string foo.bar.5 can be
;;; parsed as either just the file specified or as specifying the
;;; file, type, and version. Therefore, we use the following rules
;;; when confronted with an ambiguous file.type.version string:
;;;
;;; - If the first character is a dot, it's part of the file. It is not
;;; considered a dot in the following rules.
;;;
;;; - If there is only one dot, it separates the file and the type.
;;;
;;; - If there are multiple dots and the stuff following the last dot
;;; is a valid version, then that is the version and the stuff between
;;; the second to last dot and the last dot is the type.
;;;
;;; Wildcard characters:
;;;
;;; If the directory, file, type components contain any of the
;;; following characters, it is considered part of a wildcard pattern
;;; and has the following meaning.
;;;
;;; ? - matches any character
;;; * - matches any zero or more characters.
;;; [abc] - matches any of a, b, or c.
;;; {str1,str2,...,strn} - matches any of str1, str2, ..., or strn.
;;;
;;; Any of these special characters can be preceded by a backslash to
;;; cause it to be treated as a regular character.
(defun remove-backslashes (namestr start end)
  #!+sb-doc
  "Remove any occurrences of #\\ from the string because we've already
   checked for whatever they may have protected."
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((result (make-string (- end start) :element-type 'base-char))
	 (dst 0)
	 (quoted nil))
    (do ((src start (1+ src)))
	((= src end))
      (cond (quoted
	     (setf (schar result dst) (schar namestr src))
	     (setf quoted nil)
	     (incf dst))
	    (t
	     (let ((char (schar namestr src)))
	       (cond ((char= char #\\)
		      (setq quoted t))
		     (t
		      (setf (schar result dst) char)
		      (incf dst)))))))
    (when quoted
      (error 'namestring-parse-error
	     :complaint "backslash in a bad place"
	     :namestring namestr
	     :offset (1- end)))
    (shrink-vector result dst)))

(defvar *ignore-wildcards* nil)

(/show0 "filesys.lisp 86")

(defun maybe-make-pattern (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (if *ignore-wildcards*
      (subseq namestr start end)
      (collect ((pattern))
	(let ((quoted nil)
	      (any-quotes nil)
	      (last-regular-char nil)
	      (index start))
	  (flet ((flush-pending-regulars ()
		   (when last-regular-char
		     (pattern (if any-quotes
				  (remove-backslashes namestr
						      last-regular-char
						      index)
				  (subseq namestr last-regular-char index)))
		     (setf any-quotes nil)
		     (setf last-regular-char nil))))
	    (loop
	      (when (>= index end)
		(return))
	      (let ((char (schar namestr index)))
		(cond (quoted
		       (incf index)
		       (setf quoted nil))
		      ((char= char #\\)
		       (setf quoted t)
		       (setf any-quotes t)
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index))
		      ((char= char #\?)
		       (flush-pending-regulars)
		       (pattern :single-char-wild)
		       (incf index))
		      ((char= char #\*)
		       (flush-pending-regulars)
		       (pattern :multi-char-wild)
		       (incf index))
		      ((char= char #\[)
		       (flush-pending-regulars)
		       (let ((close-bracket
			      (position #\] namestr :start index :end end)))
			 (unless close-bracket
			   (error 'namestring-parse-error
				  :complaint "#\\[ with no corresponding #\\]"
				  :namestring namestr
				  :offset index))
			 (pattern (list :character-set
					(subseq namestr
						(1+ index)
						close-bracket)))
			 (setf index (1+ close-bracket))))
		      (t
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index)))))
	    (flush-pending-regulars)))
	(cond ((null (pattern))
	       "")
	      ((null (cdr (pattern)))
	       (let ((piece (first (pattern))))
		 (typecase piece
		   ((member :multi-char-wild) :wild)
		   (simple-string piece)
		   (t
		    (make-pattern (pattern))))))
	      (t
	       (make-pattern (pattern)))))))

(/show0 "filesys.lisp 160")

(defun extract-name-type-and-version (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((last-dot (position #\. namestr :start (1+ start) :end end
			     :from-end t)))
    (cond 
      (last-dot
       (values (maybe-make-pattern namestr start last-dot)
	       (maybe-make-pattern namestr (1+ last-dot) end)
	       :newest))
      (t
       (values (maybe-make-pattern namestr start end)
	       nil
	       :newest)))))

(/show0 "filesys.lisp 200")

;;; Take a string and return a list of cons cells that mark the char
;;; separated subseq. The first value is true if absolute directories
;;; location.
(defun split-at-slashes (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((absolute (and (/= start end)
		       (char= (schar namestr start) #\/))))
    (when absolute
      (incf start))
    ;; Next, split the remainder into slash-separated chunks.
    (collect ((pieces))
      (loop
	(let ((slash (position #\/ namestr :start start :end end)))
	  (pieces (cons start (or slash end)))
	  (unless slash
	    (return))
	  (setf start (1+ slash))))
      (values absolute (pieces)))))

(defun parse-unix-namestring (namestr start end)
  (declare (type simple-base-string namestr)
           (type index start end))
  (multiple-value-bind (absolute pieces) (split-at-slashes namestr start end)
    (multiple-value-bind (name type version)
	(let* ((tail (car (last pieces)))
	       (tail-start (car tail))
	       (tail-end (cdr tail)))
	  (unless (= tail-start tail-end)
	    (setf pieces (butlast pieces))
	    (extract-name-type-and-version namestr tail-start tail-end)))

      (when (stringp name)
	(let ((position (position-if (lambda (char)
				       (or (char= char (code-char 0))
					   (char= char #\/)))
				     name)))
	  (when position
	    (error 'namestring-parse-error
		   :complaint "can't embed #\\Nul or #\\/ in Unix namestring"
		   :namestring namestr
		   :offset position))))
      ;; Now we have everything we want. So return it.
      (values nil ; no host for Unix namestrings
	      nil ; no device for Unix namestrings
	      (collect ((dirs))
 	        (dolist (piece pieces)
		  (let ((piece-start (car piece))
			(piece-end (cdr piece)))
		    (unless (= piece-start piece-end)
		      (cond ((string= namestr ".."
				      :start1 piece-start
				      :end1 piece-end)
			     (dirs :up))
			    ((string= namestr "**"
				      :start1 piece-start
				      :end1 piece-end)
			     (dirs :wild-inferiors))
			    (t
			     (dirs (maybe-make-pattern namestr
						       piece-start
						       piece-end)))))))
		(cond (absolute
		       (cons :absolute (dirs)))
		      ((dirs)
		       (cons :relative (dirs)))
		      (t
		       nil)))
	      name
	      type
	      version))))

(/show0 "filesys.lisp 300")

(defun unparse-unix-host (pathname)
  (declare (type pathname pathname)
	   (ignore pathname))
  ;; this host designator needs to be recognized as a physical host in
  ;; PARSE-NAMESTRING. Until sbcl-0.7.3.x, we had "Unix" here, but
  ;; that's a valid Logical Hostname, so that's a bad choice. -- CSR,
  ;; 2002-05-09
  "")

(defun unparse-unix-piece (thing)
  (etypecase thing
    ((member :wild) "*")
    (simple-string
     (let* ((srclen (length thing))
	    (dstlen srclen))
       (dotimes (i srclen)
	 (case (schar thing i)
	   ((#\* #\? #\[)
	    (incf dstlen))))
       (let ((result (make-string dstlen))
	     (dst 0))
	 (dotimes (src srclen)
	   (let ((char (schar thing src)))
	     (case char
	       ((#\* #\? #\[)
		(setf (schar result dst) #\\)
		(incf dst)))
	     (setf (schar result dst) char)
	     (incf dst)))
	 result)))
    (pattern
     (collect ((strings))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string
	    (strings piece))
	   (symbol
	    (ecase piece
	      (:multi-char-wild
	       (strings "*"))
	      (:single-char-wild
	       (strings "?"))))
	   (cons
	    (case (car piece)
	      (:character-set
	       (strings "[")
	       (strings (cdr piece))
	       (strings "]"))
	      (t
	       (error "invalid pattern piece: ~S" piece))))))
       (apply #'concatenate
	      'simple-string
	      (strings))))))

(defun unparse-unix-directory-list (directory)
  (declare (type list directory))
  (collect ((pieces))
    (when directory
      (ecase (pop directory)
	(:absolute
	 (pieces "/"))
	(:relative
	 ;; nothing special
	 ))
      (dolist (dir directory)
	(typecase dir
	  ((member :up)
	   (pieces "../"))
	  ((member :back)
	   (error ":BACK cannot be represented in namestrings."))
	  ((member :wild-inferiors)
	   (pieces "**/"))
	  ((or simple-string pattern)
	   (pieces (unparse-unix-piece dir))
	   (pieces "/"))
	  (t
	   (error "invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (pieces))))

(defun unparse-unix-directory (pathname)
  (declare (type pathname pathname))
  (unparse-unix-directory-list (%pathname-directory pathname)))

(defun unparse-unix-file (pathname)
  (declare (type pathname pathname))
  (collect ((strings))
    (let* ((name (%pathname-name pathname))
	   (type (%pathname-type pathname))
	   (type-supplied (not (or (null type) (eq type :unspecific)))))
      ;; Note: by ANSI 19.3.1.1.5, we ignore the version slot when
      ;; translating logical pathnames to a filesystem without
      ;; versions (like Unix).
      (when name
	(when (and (null type) (position #\. name :start 1))
	  (error "too many dots in the name: ~S" pathname))
	(strings (unparse-unix-piece name)))
      (when type-supplied
	(unless name
	  (error "cannot specify the type without a file: ~S" pathname))
	(when (typep type 'simple-base-string)
	  (when (position #\. type)
	    (error "type component can't have a #\. inside: ~S" pathname)))
	(strings ".")
	(strings (unparse-unix-piece type))))
    (apply #'concatenate 'simple-string (strings))))

(/show0 "filesys.lisp 406")

(defun unparse-unix-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
	       (unparse-unix-directory pathname)
	       (unparse-unix-file pathname)))

(defun unparse-unix-enough (pathname defaults)
  (declare (type pathname pathname defaults))
  (flet ((lose ()
	   (error "~S cannot be represented relative to ~S."
		  pathname defaults)))
    (collect ((strings))
      (let* ((pathname-directory (%pathname-directory pathname))
	     (defaults-directory (%pathname-directory defaults))
	     (prefix-len (length defaults-directory))
	     (result-directory
	      (cond ((and (> prefix-len 1)
			  (>= (length pathname-directory) prefix-len)
			  (compare-component (subseq pathname-directory
						     0 prefix-len)
					     defaults-directory))
		     ;; Pathname starts with a prefix of default. So
		     ;; just use a relative directory from then on out.
		     (cons :relative (nthcdr prefix-len pathname-directory)))
		    ((eq (car pathname-directory) :absolute)
		     ;; We are an absolute pathname, so we can just use it.
		     pathname-directory)
		    (t
		     ;; We are a relative directory. So we lose.
		     (lose)))))
	(strings (unparse-unix-directory-list result-directory)))
      (let* ((pathname-type (%pathname-type pathname))
	     (type-needed (and pathname-type
			       (not (eq pathname-type :unspecific))))
	     (pathname-name (%pathname-name pathname))
	     (name-needed (or type-needed
			      (and pathname-name
				   (not (compare-component pathname-name
							   (%pathname-name
							    defaults)))))))
	(when name-needed
	  (unless pathname-name (lose))
	  (when (and (null pathname-type)
		     (position #\. pathname-name :start 1))
	    (error "too many dots in the name: ~S" pathname))
	  (strings (unparse-unix-piece pathname-name)))
	(when type-needed
	  (when (or (null pathname-type) (eq pathname-type :unspecific))
	    (lose))
	  (when (typep pathname-type 'simple-base-string)
	    (when (position #\. pathname-type)
	      (error "type component can't have a #\. inside: ~S" pathname)))
	  (strings ".")
	  (strings (unparse-unix-piece pathname-type))))
      (apply #'concatenate 'simple-string (strings)))))

;;;; wildcard matching stuff

;;; Return a list of all the Lispy filenames (not including e.g. the
;;; Unix magic "." and "..") in the directory named by DIRECTORY-NAME.
(defun directory-lispy-filenames (directory-name)
  (with-alien ((adlf (* c-string)
		     (alien-funcall (extern-alien
				     "alloc_directory_lispy_filenames"
				     (function (* c-string) c-string))
				    directory-name)))
    (if (null-alien adlf)
	(error 'simple-file-error
	       :pathname directory-name
	       :format-control "~@<couldn't read directory ~S: ~2I~_~A~:>"
	       :format-arguments (list directory-name (strerror)))
	(unwind-protect
	    (c-strings->string-list adlf)
	  (alien-funcall (extern-alien "free_directory_lispy_filenames"
				       (function void (* c-string)))
			 adlf)))))

(/show0 "filesys.lisp 498")

(defmacro !enumerate-matches ((var pathname &optional result
				   &key (verify-existence t)
				   (follow-links t))
			      &body body)
  `(block nil
     (%enumerate-matches (pathname ,pathname)
			 ,verify-existence
			 ,follow-links
			 (lambda (,var) ,@body))
     ,result))

(/show0 "filesys.lisp 500")

;;; Call FUNCTION on matches.
(defun %enumerate-matches (pathname verify-existence follow-links function)
  (/noshow0 "entering %ENUMERATE-MATCHES")
  (when (pathname-type pathname)
    (unless (pathname-name pathname)
      (error "cannot supply a type without a name:~%  ~S" pathname)))
  (when (and (integerp (pathname-version pathname))
	     (member (pathname-type pathname) '(nil :unspecific)))
    (error "cannot supply a version without a type:~%  ~S" pathname))
  (let ((directory (pathname-directory pathname)))
    (/noshow0 "computed DIRECTORY")
    (if directory
	(ecase (first directory)
	  (:absolute
	   (/noshow0 "absolute directory")
	   (%enumerate-directories "/" (rest directory) pathname
				   verify-existence follow-links
				   nil function))
	  (:relative
	   (/noshow0 "relative directory")
	   (%enumerate-directories "" (rest directory) pathname
				   verify-existence follow-links
				   nil function)))
	(%enumerate-files "" pathname verify-existence function))))

;;; Call FUNCTION on directories.
(defun %enumerate-directories (head tail pathname verify-existence
			       follow-links nodes function)
  (declare (simple-string head))
  (macrolet ((unix-xstat (name)
	       `(if follow-links
		    (sb!unix:unix-stat ,name)
		    (sb!unix:unix-lstat ,name)))
	     (with-directory-node-noted ((head) &body body)
	       `(multiple-value-bind (res dev ino mode)
		    (unix-xstat ,head)
		  (when (and res (eql (logand mode sb!unix:s-ifmt)
				      sb!unix:s-ifdir))
		    (let ((nodes (cons (cons dev ino) nodes)))
		      ,@body))))
	     (with-directory-node-removed ((head) &body body)
	       `(multiple-value-bind (res dev ino mode)
		    (unix-xstat ,head)
		  (when (and res (eql (logand mode sb!unix:s-ifmt)
				      sb!unix:s-ifdir))
		    (let ((nodes (remove (cons dev ino) nodes :test #'equal)))
		      ,@body)))))
    (if tail
	(let ((piece (car tail)))
	  (etypecase piece
	    (simple-string
	     (let ((head (concatenate 'base-string head piece)))
	       (with-directory-node-noted (head)
		 (%enumerate-directories (concatenate 'base-string head "/")
					 (cdr tail) pathname
					 verify-existence follow-links
					 nodes function))))
	    ((member :wild-inferiors)
	     ;; now with extra error case handling from CLHS
	     ;; 19.2.2.4.3 -- CSR, 2004-01-24
	     (when (member (cadr tail) '(:up :back))
	       (error 'simple-file-error
		      :pathname pathname
		      :format-control "~@<invalid use of ~S after :WILD-INFERIORS~@:>."
		      :format-arguments (list (cadr tail))))
	     (%enumerate-directories head (rest tail) pathname
				     verify-existence follow-links
				     nodes function)
	     (dolist (name (ignore-errors (directory-lispy-filenames head)))
	       (let ((subdir (concatenate 'base-string head name)))
		 (multiple-value-bind (res dev ino mode)
		     (unix-xstat subdir)
		   (declare (type (or fixnum null) mode))
		   (when (and res (eql (logand mode sb!unix:s-ifmt)
				       sb!unix:s-ifdir))
		     (unless (dolist (dir nodes nil)
			       (when (and (eql (car dir) dev)
					  (eql (cdr dir) ino))
				 (return t)))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'base-string subdir "/")))
			 (%enumerate-directories subdir tail pathname
						 verify-existence follow-links
						 nodes function))))))))
	    ((or pattern (member :wild))
	     (dolist (name (directory-lispy-filenames head))
	       (when (or (eq piece :wild) (pattern-matches piece name))
		 (let ((subdir (concatenate 'base-string head name)))
		   (multiple-value-bind (res dev ino mode)
		       (unix-xstat subdir)
		     (declare (type (or fixnum null) mode))
		     (when (and res
				(eql (logand mode sb!unix:s-ifmt)
				     sb!unix:s-ifdir))
		       (let ((nodes (cons (cons dev ino) nodes))
			     (subdir (concatenate 'base-string subdir "/")))
			 (%enumerate-directories subdir (rest tail) pathname
						 verify-existence follow-links
						 nodes function))))))))
	  ((member :up)
	   (when (string= head "/")
	     (error 'simple-file-error
		    :pathname pathname
		    :format-control "~@<invalid use of :UP after :ABSOLUTE.~@:>"))
	   (with-directory-node-removed (head)
	     (let ((head (concatenate 'base-string head "..")))
	       (with-directory-node-noted (head)
		 (%enumerate-directories (concatenate 'base-string head "/")
					 (rest tail) pathname
					 verify-existence follow-links
					 nodes function)))))
	  ((member :back)
	   ;; :WILD-INFERIORS is handled above, so the only case here
	   ;; should be (:ABSOLUTE :BACK)
	   (aver (string= head "/"))
	   (error 'simple-file-error
		  :pathname pathname
		  :format-control "~@<invalid use of :BACK after :ABSOLUTE.~@:>"))))
	(%enumerate-files head pathname verify-existence function))))

;;; Call FUNCTION on files.
(defun %enumerate-files (directory pathname verify-existence function)
  (declare (simple-string directory))
  (/noshow0 "entering %ENUMERATE-FILES")
  (let ((name (%pathname-name pathname))
	(type (%pathname-type pathname))
	(version (%pathname-version pathname)))
    (/noshow0 "computed NAME, TYPE, and VERSION")
    (cond ((member name '(nil :unspecific))
	   (/noshow0 "UNSPECIFIC, more or less")
	   (when (or (not verify-existence)
		     (sb!unix:unix-file-kind directory))
	     (funcall function directory)))
	  ((or (pattern-p name)
	       (pattern-p type)
	       (eq name :wild)
	       (eq type :wild))
	   (/noshow0 "WILD, more or less")
	   ;; I IGNORE-ERRORS here just because the original CMU CL
	   ;; code did. I think the intent is that it's not an error
	   ;; to request matches to a wild pattern when no matches
	   ;; exist, but I haven't tried to figure out whether
	   ;; everything is kosher. (E.g. what if we try to match a
	   ;; wildcard but we don't have permission to read one of the
	   ;; relevant directories?) -- WHN 2001-04-17
	   (dolist (complete-filename (ignore-errors
					(directory-lispy-filenames directory)))
	     (multiple-value-bind
		 (file-name file-type file-version)
		 (let ((*ignore-wildcards* t))
		   (extract-name-type-and-version
		    complete-filename 0 (length complete-filename)))
	       (when (and (components-match file-name name)
			  (components-match file-type type)
			  (components-match file-version version))
		 (funcall function
			  (concatenate 'base-string
				       directory
				       complete-filename))))))
	  (t
	   (/noshow0 "default case")
	   (let ((file (concatenate 'base-string directory name)))
	     (/noshow "computed basic FILE")
	     (unless (or (null type) (eq type :unspecific))
	       (/noshow0 "tweaking FILE for more-or-less-:UNSPECIFIC case")
	       (setf file (concatenate 'base-string file "." type)))
	     (unless (member version '(nil :newest :wild :unspecific))
	       (/noshow0 "tweaking FILE for more-or-less-:WILD case")
	       (setf file (concatenate 'base-string file "."
				       (quick-integer-to-string version))))
	     (/noshow0 "finished possibly tweaking FILE")
	     (when (or (not verify-existence)
		       (sb!unix:unix-file-kind file t))
	       (/noshow0 "calling FUNCTION on FILE")
	       (funcall function file)))))))

(/noshow0 "filesys.lisp 603")

;;; FIXME: Why do we need this?
(defun quick-integer-to-string (n)
  (declare (type integer n))
  (cond ((not (fixnump n))
	 (write-to-string n :base 10 :radix nil))
	((zerop n) "0")
	((eql n 1) "1")
	((minusp n)
	 (concatenate 'simple-base-string "-"
		      (the simple-base-string (quick-integer-to-string (- n)))))
	(t
	 (do* ((len (1+ (truncate (integer-length n) 3)))
	       (res (make-string len :element-type 'base-char))
	       (i (1- len) (1- i))
	       (q n)
	       (r 0))
	      ((zerop q)
	       (incf i)
	       (replace res res :start2 i :end2 len)
	       (shrink-vector res (- len i)))
	   (declare (simple-string res)
		    (fixnum len i r q))
	   (multiple-value-setq (q r) (truncate q 10))
	   (setf (schar res i) (schar "0123456789" r))))))

;;;; UNIX-NAMESTRING

(defun empty-relative-pathname-spec-p (x)
  (or (equal x "")
      (and (pathnamep x)
	   (or (equal (pathname-directory x) '(:relative))
	       ;; KLUDGE: I'm not sure this second check should really
	       ;; have to be here. But on sbcl-0.6.12.7,
	       ;; (PATHNAME-DIRECTORY (PATHNAME "")) is NIL, and
	       ;; (PATHNAME "") seems to act like an empty relative
	       ;; pathname, so in order to work with that, I test
	       ;; for NIL here. -- WHN 2001-05-18
	       (null (pathname-directory x)))
	   (null (pathname-name x))
	   (null (pathname-type x)))
      ;; (The ANSI definition of "pathname specifier" has 
      ;; other cases, but none of them seem to admit the possibility
      ;; of being empty and relative.)
      ))

;;; Convert PATHNAME into a string that can be used with UNIX system
;;; calls, or return NIL if no match is found. Wild-cards are expanded.
(defun unix-namestring (pathname-spec &optional (for-input t))
  (let* ((namestring (physicalize-pathname (merge-pathnames pathname-spec)))
	 (matches nil)) ; an accumulator for actual matches
    (when (wild-pathname-p namestring)
      (error 'simple-file-error
	     :pathname namestring
	     :format-control "bad place for a wild pathname"))
    (!enumerate-matches (match namestring nil :verify-existence for-input)
			(push match matches))
    (case (length matches)
      (0 nil)
      (1 (first matches))
      (t (bug "!ENUMERATE-MATCHES returned more than one match on a non-wild pathname")))))

;;;; TRUENAME and PROBE-FILE

;;; This is only trivially different from PROBE-FILE, which is silly
;;; but ANSI.
(defun truename (pathname)
  #!+sb-doc
  "Return the pathname for the actual file described by PATHNAME.
  An error of type FILE-ERROR is signalled if no such file exists,
  or the pathname is wild.

  Under Unix, the TRUENAME of a broken symlink is considered to be
  the name of the broken symlink itself."
  (let ((result (probe-file pathname)))
    (unless result
      (error 'simple-file-error
	     :pathname pathname
	     :format-control "The file ~S does not exist."
	     :format-arguments (list (namestring pathname))))
    result))

(defun probe-file (pathname)
  #!+sb-doc
  "Return a pathname which is the truename of the file if it exists, or NIL
  otherwise. An error of type FILE-ERROR is signaled if pathname is wild."
  (let* ((defaulted-pathname (merge-pathnames
			      pathname
			      (sane-default-pathname-defaults)))
	 (namestring (unix-namestring defaulted-pathname t)))
    (when (and namestring (sb!unix:unix-file-kind namestring t))
      (let ((trueishname (sb!unix:unix-resolve-links namestring)))
	(when trueishname
	  (let* ((*ignore-wildcards* t)
		 (name (sb!unix:unix-simplify-pathname trueishname))) 
	    (if (eq (sb!unix:unix-file-kind name) :directory)
		(pathname (concatenate 'string name "/"))
		(pathname name))))))))

;;;; miscellaneous other operations

(/show0 "filesys.lisp 700")

(defun rename-file (file new-name)
  #!+sb-doc
  "Rename FILE to have the specified NEW-NAME. If FILE is a stream open to a
  file, then the associated file is renamed."
  (let* ((original (truename file))
	 (original-namestring (unix-namestring original t))
	 (new-name (merge-pathnames new-name original))
	 (new-namestring (unix-namestring new-name nil)))
    (unless new-namestring
      (error 'simple-file-error
	     :pathname new-name
	     :format-control "~S can't be created."
	     :format-arguments (list new-name)))
    (multiple-value-bind (res error)
	(sb!unix:unix-rename original-namestring new-namestring)
      (unless res
	(error 'simple-file-error
	       :pathname new-name
	       :format-control "~@<couldn't rename ~2I~_~A ~I~_to ~2I~_~A: ~
                                ~I~_~A~:>"
	       :format-arguments (list original new-name (strerror error))))
      (when (streamp file)
	(file-name file new-name))
      (values new-name original (truename new-name)))))

(defun delete-file (file)
  #!+sb-doc
  "Delete the specified FILE."
  (let ((namestring (unix-namestring file t)))
    (when (streamp file)
      (close file :abort t))
    (unless namestring
      (error 'simple-file-error
	     :pathname file
	     :format-control "~S doesn't exist."
	     :format-arguments (list file)))
    (multiple-value-bind (res err) (sb!unix:unix-unlink namestring)
      (unless res
	(simple-file-perror "couldn't delete ~A" namestring err))))
  t)

;;; (This is an ANSI Common Lisp function.) 
(defun user-homedir-pathname (&optional host)
  "Return the home directory of the user as a pathname."
  (declare (ignore host))
  (pathname (sb!unix:uid-homedir (sb!unix:unix-getuid))))

(defun file-write-date (file)
  #!+sb-doc
  "Return file's creation date, or NIL if it doesn't exist.
 An error of type file-error is signaled if file is a wild pathname"
  (let ((name (unix-namestring file t)))
    (when name
      (multiple-value-bind
	    (res dev ino mode nlink uid gid rdev size atime mtime)
	  (sb!unix:unix-stat name)
	(declare (ignore dev ino mode nlink uid gid rdev size atime))
	(when res
	  (+ unix-to-universal-time mtime))))))

(defun file-author (file)
  #!+sb-doc
  "Return the file author as a string, or NIL if the author cannot be
 determined. Signal an error of type FILE-ERROR if FILE doesn't exist,
 or FILE is a wild pathname."
  (let ((name (unix-namestring (pathname file) t)))
    (unless name
      (error 'simple-file-error
	     :pathname file
	     :format-control "~S doesn't exist."
	     :format-arguments (list file)))
    (multiple-value-bind (winp dev ino mode nlink uid)
	(sb!unix:unix-stat name)
      (declare (ignore dev ino mode nlink))
      (and winp (sb!unix:uid-username uid)))))

;;;; DIRECTORY

(/show0 "filesys.lisp 800")

(defun directory (pathname &key)
  #!+sb-doc
  "Return a list of PATHNAMEs, each the TRUENAME of a file that matched the
   given pathname. Note that the interaction between this ANSI-specified
   TRUENAMEing and the semantics of the Unix filesystem (symbolic links..)
   means this function can sometimes return files which don't have the same
   directory as PATHNAME."
  (let (;; We create one entry in this hash table for each truename,
	;; as an asymptotically efficient way of removing duplicates
	;; (which can arise when e.g. multiple symlinks map to the
	;; same truename).
	(truenames (make-hash-table :test #'equal))
        (merged-pathname (merge-pathnames pathname)))
    (!enumerate-matches (match merged-pathname)
      (let* ((*ignore-wildcards* t)
	     ;; FIXME: Why not TRUENAME?  As reported by Milan Zamazal
	     ;; sbcl-devel 2003-10-05, using TRUENAME causes a race
	     ;; condition whereby removal of a file during the
	     ;; directory operation causes an error.  It's not clear
	     ;; what the right thing to do is, though.  -- CSR,
	     ;; 2003-10-13
	     (truename (probe-file match)))
	(when truename
	  (setf (gethash (namestring truename) truenames)
		truename))))
    (mapcar #'cdr
	    ;; Sorting isn't required by the ANSI spec, but sorting
	    ;; into some canonical order seems good just on the
	    ;; grounds that the implementation should have repeatable
	    ;; behavior when possible.
            (sort (loop for name being each hash-key in truenames
			using (hash-value truename)
                        collect (cons name truename))
                  #'string<
		  :key #'car))))

(/show0 "filesys.lisp 899")

;;; predicate to order pathnames by; goes by name
(defun pathname-order (x y)
  (let ((xn (%pathname-name x))
	(yn (%pathname-name y)))
    (if (and xn yn)
	(let ((res (string-lessp xn yn)))
	  (cond ((not res) nil)
		((= res (length (the simple-string xn))) t)
		((= res (length (the simple-string yn))) nil)
		(t t)))
	xn)))

(defun ensure-directories-exist (pathspec &key verbose (mode #o777))
  #!+sb-doc
  "Test whether the directories containing the specified file
  actually exist, and attempt to create them if they do not.
  The MODE argument is a CMUCL/SBCL-specific extension to control
  the Unix permission bits."
  (let ((pathname (physicalize-pathname (pathname pathspec)))
	(created-p nil))
    (when (wild-pathname-p pathname)
      (error 'simple-file-error
	     :format-control "bad place for a wild pathname"
	     :pathname pathspec))
    (let ((dir (pathname-directory pathname)))
      (loop for i from 1 upto (length dir)
	    do (let ((newpath (make-pathname
			       :host (pathname-host pathname)
			       :device (pathname-device pathname)
			       :directory (subseq dir 0 i))))
		 (unless (probe-file newpath)
		   (let ((namestring (namestring newpath)))
		     (when verbose
		       (format *standard-output*
			       "~&creating directory: ~A~%"
			       namestring))
		     (sb!unix:unix-mkdir namestring mode)
		     (unless (probe-file namestring)
		       (error 'simple-file-error
			      :pathname pathspec
			      :format-control "can't create directory ~A"
			      :format-arguments (list namestring)))
		     (setf created-p t)))))
      (values pathname created-p))))

(/show0 "filesys.lisp 1000")
