;;;; machine/filesystem-independent pathname functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

#!-sb-fluid (declaim (freeze-type logical-pathname logical-host))

;;; host methods

(def!method print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity t)))

;;; pathname methods

(def!method print-object ((pathname pathname) stream)
  (let ((namestring (handler-case (namestring pathname)
		      (error nil))))
    (if namestring
	(format stream "#P~S" namestring)
	;; FIXME: This code was rewritten and should be tested. (How does
	;; control get to this case anyhow? Perhaps we could just punt it?)
	(print-unreadable-object (pathname stream :type t)
	  (format stream
		  "(with no namestring) :HOST ~S :DEVICE ~S :DIRECTORY ~S ~
		  :NAME ~S :TYPE ~S :VERSION ~S"
		  (%pathname-host pathname)
		  (%pathname-device pathname)
		  (%pathname-directory pathname)
		  (%pathname-name pathname)
		  (%pathname-type pathname)
		  (%pathname-version pathname))))))

(def!method make-load-form ((pathname pathname) &optional environment)
  (make-load-form-saving-slots pathname :environment environment))

;;; The potential conflict with search lists requires isolating the
;;; printed representation to use the i/o macro #.(logical-pathname
;;; <path-designator>).
;;;
;;; FIXME: We don't use search lists any more, so that comment is
;;; stale, right?
(def!method print-object ((pathname logical-pathname) stream)
  (let ((namestring (handler-case (namestring pathname)
		      (error nil))))
    (if namestring
	(format stream "#.(logical-pathname ~S)" namestring)
	(print-unreadable-object (pathname stream :type t)
	  (format stream
		  ":HOST ~S :DIRECTORY ~S :FILE ~S :NAME=~S :VERSION ~S"
		  (%pathname-host pathname)
		  (%pathname-directory pathname)
		  (%pathname-name pathname)
		  (%pathname-type pathname)
		  (%pathname-version pathname))))))

;;; A pathname is logical if the host component is a logical host.
;;; This constructor is used to make an instance of the correct type
;;; from parsed arguments.
(defun %make-maybe-logical-pathname (host device directory name type version)
  ;; We canonicalize logical pathname components to uppercase. ANSI
  ;; doesn't strictly require this, leaving it up to the implementor;
  ;; but the arguments given in the X3J13 cleanup issue
  ;; PATHNAME-LOGICAL:ADD seem compelling: we should canonicalize the
  ;; case, and uppercase is the ordinary way to do that.
  (flet ((upcase-maybe (x) (typecase x (string (string-upcase x)) (t x))))
    (if (typep host 'logical-host)
	(%make-logical-pathname host
				:unspecific
				(mapcar #'upcase-maybe directory)
				(upcase-maybe name)
				(upcase-maybe type)
				version)
	(%make-pathname host device directory name type version))))

;;; Hash table searching maps a logical pathname's host to its
;;; physical pathname translation.
(defvar *logical-hosts* (make-hash-table :test 'equal))

;;;; patterns

(def!method make-load-form ((pattern pattern) &optional environment)
  (make-load-form-saving-slots pattern :environment environment))

(def!method print-object ((pattern pattern) stream)
  (print-unreadable-object (pattern stream :type t)
    (if *print-pretty*
	(let ((*print-escape* t))
	  (pprint-fill stream (pattern-pieces pattern) nil))
	(prin1 (pattern-pieces pattern) stream))))

(defun pattern= (pattern1 pattern2)
  (declare (type pattern pattern1 pattern2))
  (let ((pieces1 (pattern-pieces pattern1))
	(pieces2 (pattern-pieces pattern2)))
    (and (= (length pieces1) (length pieces2))
	 (every #'(lambda (piece1 piece2)
		    (typecase piece1
		      (simple-string
		       (and (simple-string-p piece2)
			    (string= piece1 piece2)))
		      (cons
		       (and (consp piece2)
			    (eq (car piece1) (car piece2))
			    (string= (cdr piece1) (cdr piece2))))
		      (t
		       (eq piece1 piece2))))
		pieces1
		pieces2))))

;;; If the string matches the pattern returns the multiple values T and a
;;; list of the matched strings.
(defun pattern-matches (pattern string)
  (declare (type pattern pattern)
	   (type simple-string string))
  (let ((len (length string)))
    (labels ((maybe-prepend (subs cur-sub chars)
	       (if cur-sub
		   (let* ((len (length chars))
			  (new (make-string len))
			  (index len))
		     (dolist (char chars)
		       (setf (schar new (decf index)) char))
		     (cons new subs))
		   subs))
	     (matches (pieces start subs cur-sub chars)
	       (if (null pieces)
		   (if (= start len)
		       (values t (maybe-prepend subs cur-sub chars))
		       (values nil nil))
		   (let ((piece (car pieces)))
		     (etypecase piece
		       (simple-string
			(let ((end (+ start (length piece))))
			  (and (<= end len)
			       (string= piece string
					:start2 start :end2 end)
			       (matches (cdr pieces) end
					(maybe-prepend subs cur-sub chars)
					nil nil))))
		       (list
			(ecase (car piece)
			  (:character-set
			   (and (< start len)
				(let ((char (schar string start)))
				  (if (find char (cdr piece) :test #'char=)
				      (matches (cdr pieces) (1+ start) subs t
					       (cons char chars))))))))
		       ((member :single-char-wild)
			(and (< start len)
			     (matches (cdr pieces) (1+ start) subs t
				      (cons (schar string start) chars))))
		       ((member :multi-char-wild)
			(multiple-value-bind (won new-subs)
			    (matches (cdr pieces) start subs t chars)
			  (if won
			      (values t new-subs)
			      (and (< start len)
				   (matches pieces (1+ start) subs t
					    (cons (schar string start)
						  chars)))))))))))
      (multiple-value-bind (won subs)
	  (matches (pattern-pieces pattern) 0 nil nil nil)
	(values won (reverse subs))))))

;;; PATHNAME-MATCH-P for directory components
(defun directory-components-match (thing wild)
  (or (eq thing wild)
      (eq wild :wild)
      ;; If THING has a null directory, assume that it matches
      ;; (:ABSOLUTE :WILD-INFERIORS) or (:RELATIVE :WILD-INFERIORS).
      (and (consp wild)
	   (null thing)
	   (member (first wild) '(:absolute :relative))
	   (eq (second wild) :wild-inferiors))
      (and (consp wild)
	   (let ((wild1 (first wild)))
	     (if (eq wild1 :wild-inferiors)
		 (let ((wild-subdirs (rest wild)))
		   (or (null wild-subdirs)
		       (loop
			 (when (directory-components-match thing wild-subdirs)
			   (return t))
			 (pop thing)
			 (unless thing (return nil)))))
		 (and (consp thing)
		      (components-match (first thing) wild1)
		      (directory-components-match (rest thing)
						  (rest wild))))))))

;;; Return true if pathname component THING is matched by WILD. (not
;;; commutative)
(defun components-match (thing wild)
  (declare (type (or pattern symbol simple-string integer) thing wild))
  (or (eq thing wild)
      (eq wild :wild)
      (typecase thing
	(simple-base-string
	 ;; String is matched by itself, a matching pattern or :WILD.
	 (typecase wild
	   (pattern
	    (values (pattern-matches wild thing)))
	   (simple-base-string
	    (string= thing wild))))
	(pattern
	 ;; A pattern is only matched by an identical pattern.
	 (and (pattern-p wild) (pattern= thing wild)))
	(integer
	 ;; An integer (version number) is matched by :WILD or the
	 ;; same integer. This branch will actually always be NIL as
	 ;; long as the version is a fixnum.
	 (eql thing wild)))))

;;; a predicate for comparing two pathname slot component sub-entries
(defun compare-component (this that)
  (or (eql this that)
      (typecase this
	(simple-string
	 (and (simple-string-p that)
	      (string= this that)))
	(pattern
	 (and (pattern-p that)
	      (pattern= this that)))
	(cons
	 (and (consp that)
	      (compare-component (car this) (car that))
	      (compare-component (cdr this) (cdr that)))))))

;;;; pathname functions

(defun pathname= (pathname1 pathname2)
  (declare (type pathname pathname1)
	   (type pathname pathname2))
  (and (eq (%pathname-host pathname1)
	   (%pathname-host pathname2))
       (compare-component (%pathname-device pathname1)
			  (%pathname-device pathname2))
       (compare-component (%pathname-directory pathname1)
			  (%pathname-directory pathname2))
       (compare-component (%pathname-name pathname1)
			  (%pathname-name pathname2))
       (compare-component (%pathname-type pathname1)
			  (%pathname-type pathname2))
       (compare-component (%pathname-version pathname1)
			  (%pathname-version pathname2))))

;;; Convert PATHNAME-DESIGNATOR (a pathname, or string, or
;;; stream), into a pathname in pathname.
;;;
;;; FIXME: was rewritten, should be tested (or rewritten again, this
;;; time using ONCE-ONLY, *then* tested)
;;; FIXME: become SB!XC:DEFMACRO inside EVAL-WHEN (COMPILE EVAL)?
(defmacro with-pathname ((pathname pathname-designator) &body body)
  (let ((pd0 (gensym)))
    `(let* ((,pd0 ,pathname-designator)
	    (,pathname (etypecase ,pd0
			 (pathname ,pd0)
			 (string (parse-namestring ,pd0))
			 (stream (file-name ,pd0)))))
       ,@body)))

;;; Convert the var, a host or string name for a host, into a
;;; LOGICAL-HOST structure or nil if not defined.
;;;
;;; pw notes 1/12/97 this potentially useful macro is not used anywhere
;;; and 'find-host' is not defined. 'find-logical-host' seems to be needed.
#|
(defmacro with-host ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (typecase ,var
		   (logical-host ,var)
		   (string (find-logical-host ,var nil))
		   (t nil)))))
     ,@body))
|#

(defun pathname (thing)
  #!+sb-doc
  "Convert thing (a pathname, string or stream) into a pathname."
  (declare (type pathname-designator thing))
  (with-pathname (pathname thing)
    pathname))

;;; Change the case of thing if DIDDLE-P.
(defun maybe-diddle-case (thing diddle-p)
  (if (and diddle-p (not (or (symbolp thing) (integerp thing))))
      (labels ((check-for (pred in)
		 (typecase in
		   (pattern
		    (dolist (piece (pattern-pieces in))
		      (when (typecase piece
			      (simple-string
			       (check-for pred piece))
			      (cons
			       (case (car in)
				 (:character-set
				  (check-for pred (cdr in))))))
			(return t))))
		   (list
		    (dolist (x in)
		      (when (check-for pred x)
			(return t))))
		   (simple-base-string
		    (dotimes (i (length in))
		      (when (funcall pred (schar in i))
			(return t))))
		   (t nil)))
	       (diddle-with (fun thing)
		 (typecase thing
		   (pattern
		    (make-pattern
		     (mapcar #'(lambda (piece)
				 (typecase piece
				   (simple-base-string
				    (funcall fun piece))
				   (cons
				    (case (car piece)
				      (:character-set
				       (cons :character-set
					     (funcall fun (cdr piece))))
				      (t
				       piece)))
				   (t
				    piece)))
			     (pattern-pieces thing))))
		   (list
		    (mapcar fun thing))
		   (simple-base-string
		    (funcall fun thing))
		   (t
		    thing))))
	(let ((any-uppers (check-for #'upper-case-p thing))
	      (any-lowers (check-for #'lower-case-p thing)))
	  (cond ((and any-uppers any-lowers)
		 ;; Mixed case, stays the same.
		 thing)
		(any-uppers
		 ;; All uppercase, becomes all lower case.
		 (diddle-with #'(lambda (x) (if (stringp x)
						(string-downcase x)
						x)) thing))
		(any-lowers
		 ;; All lowercase, becomes all upper case.
		 (diddle-with #'(lambda (x) (if (stringp x)
						(string-upcase x)
						x)) thing))
		(t
		 ;; No letters?  I guess just leave it.
		 thing))))
      thing))

(defun merge-directories (dir1 dir2 diddle-case)
  (if (or (eq (car dir1) :absolute)
	  (null dir2))
      dir1
      (let ((results nil))
	(flet ((add (dir)
		 (if (and (eq dir :back)
			  results
			  (not (eq (car results) :back)))
		     (pop results)
		     (push dir results))))
	  (dolist (dir (maybe-diddle-case dir2 diddle-case))
	    (add dir))
	  (dolist (dir (cdr dir1))
	    (add dir)))
	(reverse results))))

(defun merge-pathnames (pathname
			&optional
			(defaults *default-pathname-defaults*)
			(default-version :newest))
  #!+sb-doc
  "Construct a filled in pathname by completing the unspecified components
   from the defaults."
  (declare (type pathname-designator pathname)
	   (type pathname-designator defaults)
	   (values pathname))
  (with-pathname (defaults defaults)
    (let ((pathname (let ((*default-pathname-defaults* defaults))
		      (pathname pathname))))
      (let* ((default-host (%pathname-host defaults))
	     (pathname-host (%pathname-host pathname))
	     (diddle-case
	      (and default-host pathname-host
		   (not (eq (host-customary-case default-host)
			    (host-customary-case pathname-host))))))
	(%make-maybe-logical-pathname
	 (or pathname-host default-host)
	 (or (%pathname-device pathname)
	     (maybe-diddle-case (%pathname-device defaults)
				diddle-case))
	 (merge-directories (%pathname-directory pathname)
			    (%pathname-directory defaults)
			    diddle-case)
	 (or (%pathname-name pathname)
	     (maybe-diddle-case (%pathname-name defaults)
				diddle-case))
	 (or (%pathname-type pathname)
	     (maybe-diddle-case (%pathname-type defaults)
				diddle-case))
	 (or (%pathname-version pathname)
	     default-version))))))

(defun import-directory (directory diddle-case)
  (etypecase directory
    (null nil)
    ((member :wild) '(:absolute :wild-inferiors))
    ((member :unspecific) '(:relative))
    (list
     (collect ((results))
       (ecase (pop directory)
	 (:absolute
	  (results :absolute)
	  (when (search-list-p (car directory))
	    (results (pop directory))))
	 (:relative
	  (results :relative)))
       (dolist (piece directory)
	 (cond ((member piece '(:wild :wild-inferiors :up :back))
		(results piece))
	       ((or (simple-string-p piece) (pattern-p piece))
		(results (maybe-diddle-case piece diddle-case)))
	       ((stringp piece)
		(results (maybe-diddle-case (coerce piece 'simple-string)
					    diddle-case)))
	       (t
		(error "~S is not allowed as a directory component." piece))))
       (results)))
    (simple-string
     `(:absolute
       ,(maybe-diddle-case directory diddle-case)))
    (string
     `(:absolute
       ,(maybe-diddle-case (coerce directory 'simple-string)
			   diddle-case)))))

(defun make-pathname (&key host
			   (device nil devp)
			   (directory nil dirp)
			   (name nil namep)
			   (type nil typep)
			   (version nil versionp)
			   defaults
			   (case :local))
  #!+sb-doc
  "Makes a new pathname from the component arguments. Note that host is
a host-structure or string."
  (declare (type (or string host component-tokens) host)
	   (type (or string component-tokens) device)
	   (type (or list string pattern component-tokens) directory)
	   (type (or string pattern component-tokens) name type)
	   (type (or integer component-tokens (member :newest)) version)
	   (type (or pathname-designator null) defaults)
	   (type (member :common :local) case))
  (let* ((defaults (when defaults
		     (with-pathname (defaults defaults) defaults)))
	 (default-host (if defaults
			   (%pathname-host defaults)
			   (pathname-host *default-pathname-defaults*)))
	 ;; toy@rtp.ericsson.se: CLHS says make-pathname can take a
	 ;; string (as a logical-host) for the host part. We map that
	 ;; string into the corresponding logical host structure.
	 ;;
	 ;; pw@snoopy.mv.com:
	 ;; HyperSpec says for the arg to MAKE-PATHNAME;
	 ;; "host---a valid physical pathname host. ..."
	 ;; where it probably means -- a valid pathname host.
	 ;; "valid pathname host n. a valid physical pathname host or
	 ;; a valid logical pathname host."
	 ;; and defines
	 ;; "valid physical pathname host n. any of a string,
	 ;; a list of strings, or the symbol :unspecific,
	 ;; that is recognized by the implementation as the name of a host."
	 ;; "valid logical pathname host n. a string that has been defined
	 ;; as the name of a logical host. ..."
	 ;; HS is silent on what happens if the :HOST arg is NOT one of these.
	 ;; It seems an error message is appropriate.
	 (host (typecase host
		 (host host) 		; A valid host, use it.
		 (string (find-logical-host host t)) ; logical-host or lose.
		 (t default-host)))	; unix-host
	 (diddle-args (and (eq (host-customary-case host) :lower)
			   (eq case :common)))
	 (diddle-defaults
	  (not (eq (host-customary-case host)
		   (host-customary-case default-host))))
	 (dev (if devp device (if defaults (%pathname-device defaults))))
	 (dir (import-directory directory diddle-args))
	 (ver (cond
	       (versionp version)
	       (defaults (%pathname-version defaults))
	       (t nil))))
    (when (and defaults (not dirp))
      (setf dir
	    (merge-directories dir
			       (%pathname-directory defaults)
			       diddle-defaults)))

    (macrolet ((pick (var varp field)
		 `(cond ((or (simple-string-p ,var)
			     (pattern-p ,var))
			 (maybe-diddle-case ,var diddle-args))
			((stringp ,var)
			 (maybe-diddle-case (coerce ,var 'simple-string)
					    diddle-args))
			(,varp
			 (maybe-diddle-case ,var diddle-args))
			(defaults
			 (maybe-diddle-case (,field defaults)
					    diddle-defaults))
			(t
			 nil))))
      (%make-maybe-logical-pathname host
				    dev ; forced to :UNSPECIFIC when logical
				    dir
				    (pick name namep %pathname-name)
				    (pick type typep %pathname-type)
				    ver))))

(defun pathname-host (pathname &key (case :local))
  #!+sb-doc
  "Accessor for the pathname's host."
  (declare (type pathname-designator pathname)
	   (type (member :local :common) case)
	   (values host)
	   (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

(defun pathname-device (pathname &key (case :local))
  #!+sb-doc
  "Accessor for pathname's device."
  (declare (type pathname-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-device pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-directory (pathname &key (case :local))
  #!+sb-doc
  "Accessor for the pathname's directory list."
  (declare (type pathname-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-directory pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))
(defun pathname-name (pathname &key (case :local))
  #!+sb-doc
  "Accessor for the pathname's name."
  (declare (type pathname-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-name pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-type (pathname &key (case :local))
  #!+sb-doc
  "Accessor for the pathname's name."
  (declare (type pathname-designator pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-type pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-version (pathname)
  #!+sb-doc
  "Accessor for the pathname's version."
  (declare (type pathname-designator pathname))
  (with-pathname (pathname pathname)
    (%pathname-version pathname)))

;;;; namestrings

;;; Handle the case where PARSE-NAMESTRING is actually parsing a
;;; namestring. We pick off the :JUNK-ALLOWED case then find a host to
;;; use for parsing, call the parser, then check whether the host matches.
(defun %parse-namestring (namestr host defaults start end junk-allowed)
  (declare (type (or host null) host)
	   (type string namestr)
	   (type index start)
	   (type (or index null) end))
  (if junk-allowed
      (handler-case
	  (%parse-namestring namestr host defaults start end nil)
	(namestring-parse-error (condition)
	  (values nil (namestring-parse-error-offset condition))))
      (let* ((end (or end (length namestr)))
	     (parse-host (or host
			     (extract-logical-host-prefix namestr start end)
			     (pathname-host defaults))))
	(unless parse-host
	  (error "When no HOST argument is supplied, the DEFAULTS argument ~
		  must have a non-null PATHNAME-HOST."))

	(multiple-value-bind (new-host device directory file type version)
	    (funcall (host-parse parse-host) namestr start end)
	  (when (and host new-host (not (eq new-host host)))
	    (error 'simple-type-error
		   :datum new-host
		   ;; Note: ANSI requires that this be a TYPE-ERROR,
		   ;; but there seems to be no completely correct
		   ;; value to use for TYPE-ERROR-EXPECTED-TYPE.
		   ;; Instead, we return a sort of "type error allowed
		   ;; type", trying to say "it would be OK if you
		   ;; passed NIL as the host value" but not mentioning
		   ;; that a matching string would be OK too.
		   :expected-type 'null
		   :format-control
		   "The host in the namestring, ~S,~@
		    does not match the explicit HOST argument, ~S."
		   :format-arguments (list new-host host)))
	  (let ((pn-host (or new-host parse-host)))
	    (values (%make-maybe-logical-pathname
		     pn-host device directory file type version)
		    end))))))

;;; If NAMESTR begins with a colon-terminated, defined, logical host,
;;; then return that host, otherwise return NIL.
(defun extract-logical-host-prefix (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end)
	   (values (or logical-host null)))
  (let ((colon-pos (position #\: namestr :start start :end end)))
    (if colon-pos
	(values (gethash (nstring-upcase (subseq namestr start colon-pos))
			 *logical-hosts*))
	nil)))

(defun parse-namestring (thing
			 &optional
			 host
			 (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  (declare (type pathname-designator thing)
	   (type (or list host string (member :unspecific)) host)
	   (type pathname defaults)
	   (type index start)
	   (type (or index null) end)
	   (type (or t null) junk-allowed)
	   (values (or null pathname) (or null index)))
  ;; Generally, redundant specification of information in software,
  ;; whether in code or in comments, is bad. However, the ANSI spec
  ;; for this is messy enough that it's hard to hold in short-term
  ;; memory, so I've recorded these redundant notes on the
  ;; implications of the ANSI spec.
  ;; 
  ;; According to the ANSI spec, HOST can be a valid pathname host, or
  ;; a logical host, or NIL.
  ;;
  ;; A valid pathname host can be a valid physical pathname host or a
  ;; valid logical pathname host.
  ;; 
  ;; A valid physical pathname host is "any of a string, a list of
  ;; strings, or the symbol :UNSPECIFIC, that is recognized by the
  ;; implementation as the name of a host". In SBCL as of 0.6.9.8,
  ;; that means :UNSPECIFIC: though someday we might want to
  ;; generalize it to allow strings like "RTFM.MIT.EDU" or lists like
  ;; '("RTFM" "MIT" "EDU"), that's not supported now.
  ;; 
  ;; A valid logical pathname host is a string which has been defined as
  ;; the name of a logical host, as with LOAD-LOGICAL-PATHNAME-TRANSLATIONS.
  ;; 
  ;; A logical host is an object of implementation-dependent nature. In
  ;; SBCL, it's a member of the HOST class (a subclass of STRUCTURE-OBJECT).
  (let ((found-host (etypecase host
		      (string
		       ;; In general ANSI-compliant Common Lisps, a
		       ;; string might also be a physical pathname host,
		       ;; but ANSI leaves this up to the implementor,
		       ;; and in SBCL we don't do it, so it must be a
		       ;; logical host.
		       (find-logical-host host))
		      ((or null (member :unspecific))
		       ;; CLHS says that HOST=:UNSPECIFIC has
		       ;; implementation-defined behavior. We
		       ;; just turn it into NIL.
		       nil)
		      (list
		       ;; ANSI also allows LISTs to designate hosts,
		       ;; but leaves its interpretation
		       ;; implementation-defined. Our interpretation
		       ;; is that it's unsupported.:-|
		       (error "A LIST representing a pathname host is not ~
                              supported in this implementation:~%  ~S"
			      host))
		      (host
		       host))))
    (declare (type (or null host) found-host))
    (etypecase thing
      (simple-string
       (%parse-namestring thing found-host defaults start end junk-allowed))
      (string
       (%parse-namestring (coerce thing 'simple-string)
			  found-host defaults start end junk-allowed))
      (pathname
       (let ((defaulted-host (or found-host (%pathname-host defaults))))
	 (declare (type host defaulted-host))
	 (unless (eq defaulted-host (%pathname-host thing))
	   (error "The HOST argument doesn't match the pathname host:~%  ~
                  ~S and ~S."
		  defaulted-host (%pathname-host thing))))
       (values thing start))
      (stream
       (let ((name (file-name thing)))
	 (unless name
	   (error "can't figure out the file associated with stream:~%  ~S"
		  thing))
	 (values name nil))))))

(defun namestring (pathname)
  #!+sb-doc
  "Construct the full (name)string form of the pathname."
  (declare (type pathname-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (when pathname
      (let ((host (%pathname-host pathname)))
	(unless host
	  (error "can't determine the namestring for pathnames with no ~
		  host:~%  ~S" pathname))
	(funcall (host-unparse host) pathname)))))

(defun host-namestring (pathname)
  #!+sb-doc
  "Returns a string representation of the name of the host in the pathname."
  (declare (type pathname-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-host host) pathname)
	  (error
	   "can't determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun directory-namestring (pathname)
  #!+sb-doc
  "Returns a string representation of the directories used in the pathname."
  (declare (type pathname-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-directory host) pathname)
	  (error
	   "can't determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun file-namestring (pathname)
  #!+sb-doc
  "Returns a string representation of the name used in the pathname."
  (declare (type pathname-designator pathname)
	   (values (or null simple-base-string)))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-file host) pathname)
	  (error
	   "can't determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun enough-namestring (pathname
			  &optional
			  (defaults *default-pathname-defaults*))
  #!+sb-doc
  "Returns an abbreviated pathname sufficent to identify the pathname relative
   to the defaults."
  (declare (type pathname-designator pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (with-pathname (defaults defaults)
	    (funcall (host-unparse-enough host) pathname defaults))
	  (error
	   "can't determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

;;;; wild pathnames

(defun wild-pathname-p (pathname &optional field-key)
  #!+sb-doc
  "Predicate for determining whether pathname contains any wildcards."
  (declare (type pathname-designator pathname)
	   (type (member nil :host :device :directory :name :type :version)
		 field-key))
  (with-pathname (pathname pathname)
    (flet ((frob (x)
	     (or (pattern-p x) (member x '(:wild :wild-inferiors)))))
      (ecase field-key
	((nil)
	 (or (wild-pathname-p pathname :host)
	     (wild-pathname-p pathname :device)
	     (wild-pathname-p pathname :directory)
	     (wild-pathname-p pathname :name)
	     (wild-pathname-p pathname :type)
	     (wild-pathname-p pathname :version)))
	(:host (frob (%pathname-host pathname)))
	(:device (frob (%pathname-host pathname)))
	(:directory (some #'frob (%pathname-directory pathname)))
	(:name (frob (%pathname-name pathname)))
	(:type (frob (%pathname-type pathname)))
	(:version (frob (%pathname-version pathname)))))))

(defun pathname-match-p (in-pathname in-wildname)
  #!+sb-doc
  "Pathname matches the wildname template?"
  (declare (type pathname-designator in-pathname))
  (with-pathname (pathname in-pathname)
    (with-pathname (wildname in-wildname)
      (macrolet ((frob (field &optional (op 'components-match ))
		   `(or (null (,field wildname))
			(,op (,field pathname) (,field wildname)))))
	(and (or (null (%pathname-host wildname))
		 (eq (%pathname-host wildname) (%pathname-host pathname)))
	     (frob %pathname-device)
	     (frob %pathname-directory directory-components-match)
	     (frob %pathname-name)
	     (frob %pathname-type)
	     (frob %pathname-version))))))

;;; Place the substitutions into the pattern and return the string or pattern
;;; that results. If DIDDLE-CASE is true, we diddle the result case as well,
;;; in case we are translating between hosts with difference conventional case.
;;; The second value is the tail of subs with all of the values that we used up
;;; stripped off. Note that PATTERN-MATCHES matches all consecutive wildcards
;;; as a single string, so we ignore subsequent contiguous wildcards.
(defun substitute-into (pattern subs diddle-case)
  (declare (type pattern pattern)
	   (type list subs)
	   (values (or simple-base-string pattern)))
  (let ((in-wildcard nil)
	(pieces nil)
	(strings nil))
    (dolist (piece (pattern-pieces pattern))
      (cond ((simple-string-p piece)
	     (push piece strings)
	     (setf in-wildcard nil))
	    (in-wildcard)
	    (t
	     (setf in-wildcard t)
	     (unless subs
	       (error "not enough wildcards in FROM pattern to match ~
		       TO pattern:~%  ~S"
		      pattern))
	     (let ((sub (pop subs)))
	       (typecase sub
		 (pattern
		  (when strings
		    (push (apply #'concatenate 'simple-string
				 (nreverse strings))
			  pieces))
		  (dolist (piece (pattern-pieces sub))
		    (push piece pieces)))
		 (simple-string
		  (push sub strings))
		 (t
		  (error "can't substitute this into the middle of a word:~
			  ~%  ~S"
			 sub)))))))

    (when strings
      (push (apply #'concatenate 'simple-string (nreverse strings))
	    pieces))
    (values
     (maybe-diddle-case
      (if (and pieces (simple-string-p (car pieces)) (null (cdr pieces)))
	  (car pieces)
	  (make-pattern (nreverse pieces)))
      diddle-case)
     subs)))

;;; Called when we can't see how source and from matched.
(defun didnt-match-error (source from)
  (error "Pathname components from SOURCE and FROM args to TRANSLATE-PATHNAME~@
	  did not match:~%  ~S ~S"
	 source from))

;;; Do TRANSLATE-COMPONENT for all components except host and directory.
(defun translate-component (source from to diddle-case)
  (typecase to
    (pattern
     (typecase from
       (pattern
	(typecase source
	  (pattern
	   (if (pattern= from source)
	       source
	       (didnt-match-error source from)))
	  (simple-string
	   (multiple-value-bind (won subs) (pattern-matches from source)
	     (if won
		 (values (substitute-into to subs diddle-case))
		 (didnt-match-error source from))))
	  (t
	   (maybe-diddle-case source diddle-case))))
       ((member :wild)
	(values (substitute-into to (list source) diddle-case)))
       (t
	(if (components-match source from)
	    (maybe-diddle-case source diddle-case)
	    (didnt-match-error source from)))))
    ((member nil :wild)
     (maybe-diddle-case source diddle-case))
    (t
     (if (components-match source from)
	 to
	 (didnt-match-error source from)))))

;;; Return a list of all the things that we want to substitute into the TO
;;; pattern (the things matched by from on source.)  When From contains
;;; :WILD-INFERIORS, the result contains a sublist of the matched source
;;; subdirectories.
(defun compute-directory-substitutions (orig-source orig-from)
  (let ((source orig-source)
	(from orig-from))
    (collect ((subs))
      (loop
	(unless source
	  (unless (every #'(lambda (x) (eq x :wild-inferiors)) from)
	    (didnt-match-error orig-source orig-from))
	  (subs ())
	  (return))
	(unless from (didnt-match-error orig-source orig-from))
	(let ((from-part (pop from))
	      (source-part (pop source)))
	  (typecase from-part
	    (pattern
	     (typecase source-part
	       (pattern
		(if (pattern= from-part source-part)
		    (subs source-part)
		    (didnt-match-error orig-source orig-from)))
	       (simple-string
		(multiple-value-bind (won new-subs)
		    (pattern-matches from-part source-part)
		  (if won
		      (dolist (sub new-subs)
			(subs sub))
		      (didnt-match-error orig-source orig-from))))
	       (t
		(didnt-match-error orig-source orig-from))))
	    ((member :wild)
	     (subs source-part))
	    ((member :wild-inferiors)
	     (let ((remaining-source (cons source-part source)))
	       (collect ((res))
		 (loop
		   (when (directory-components-match remaining-source from)
		     (return))
		   (unless remaining-source
		     (didnt-match-error orig-source orig-from))
		   (res (pop remaining-source)))
		 (subs (res))
		 (setq source remaining-source))))
	    (simple-string
	     (unless (and (simple-string-p source-part)
			  (string= from-part source-part))
	       (didnt-match-error orig-source orig-from)))
	    (t
	     (didnt-match-error orig-source orig-from)))))
      (subs))))

;;; This is called by TRANSLATE-PATHNAME on the directory components
;;; of its argument pathnames to produce the result directory
;;; component. If this leaves the directory NIL, we return the source
;;; directory. The :RELATIVE or :ABSOLUTE is taken from the source
;;; directory, except if TO is :ABSOLUTE, in which case the result
;;; will be :ABSOLUTE.
(defun translate-directories (source from to diddle-case)
  (if (not (and source to from))
      (or (and to (null source) (remove :wild-inferiors to))
	  (mapcar (lambda (x) (maybe-diddle-case x diddle-case)) source))
      (collect ((res))
	       ;; If TO is :ABSOLUTE, the result should still be :ABSOLUTE.
	       (res (if (eq (first to) :absolute)
		 :absolute
		 (first source)))
	(let ((subs-left (compute-directory-substitutions (rest source)
							  (rest from))))
	  (dolist (to-part (rest to))
	    (typecase to-part
	      ((member :wild)
	       (aver subs-left)
	       (let ((match (pop subs-left)))
		 (when (listp match)
		   (error ":WILD-INFERIORS is not paired in from and to ~
			   patterns:~%  ~S ~S" from to))
		 (res (maybe-diddle-case match diddle-case))))
	      ((member :wild-inferiors)
	       (aver subs-left)
	       (let ((match (pop subs-left)))
		 (unless (listp match)
		   (error ":WILD-INFERIORS not paired in from and to ~
			   patterns:~%  ~S ~S" from to))
		 (dolist (x match)
		   (res (maybe-diddle-case x diddle-case)))))
	      (pattern
	       (multiple-value-bind
		   (new new-subs-left)
		   (substitute-into to-part subs-left diddle-case)
		 (setf subs-left new-subs-left)
		 (res new)))
	      (t (res to-part)))))
	(res))))

(defun translate-pathname (source from-wildname to-wildname &key)
  #!+sb-doc
  "Use the source pathname to translate the from-wildname's wild and
   unspecified elements into a completed to-pathname based on the to-wildname."
  (declare (type pathname-designator source from-wildname to-wildname))
  (with-pathname (source source)
    (with-pathname (from from-wildname)
      (with-pathname (to to-wildname)
	  (let* ((source-host (%pathname-host source))
		 (to-host (%pathname-host to))
		 (diddle-case
		  (and source-host to-host
		       (not (eq (host-customary-case source-host)
				(host-customary-case to-host))))))
	    (macrolet ((frob (field &optional (op 'translate-component))
			 `(let ((result (,op (,field source)
					     (,field from)
					     (,field to)
					     diddle-case)))
			    (if (eq result :error)
				(error "~S doesn't match ~S." source from)
				result))))
	      (%make-maybe-logical-pathname
	       (or to-host source-host)
	       (frob %pathname-device)
	       (frob %pathname-directory translate-directories)
	       (frob %pathname-name)
	       (frob %pathname-type)
	       (frob %pathname-version))))))))

;;;; search lists

(def!struct (search-list (:make-load-form-fun
			  (lambda (s)
			    (values `(intern-search-list
				      ',(search-list-name s))
				    nil))))
  ;; The name of this search-list. Always stored in lowercase.
  (name (required-argument) :type simple-string)
  ;; T if this search-list has been defined. Otherwise NIL.
  (defined nil :type (member t nil))
  ;; the list of expansions for this search-list. Each expansion is
  ;; the list of directory components to use in place of this
  ;; search-list.
  (expansions nil :type list))
(def!method print-object ((sl search-list) stream)
  (print-unreadable-object (sl stream :type t)
    (write-string (search-list-name sl) stream)))

;;; a hash table mapping search-list names to search-list structures
(defvar *search-lists* (make-hash-table :test 'equal))

;;; When search-lists are encountered in namestrings, they are
;;; converted to search-list structures right then, instead of waiting
;;; until the search list used. This allows us to verify ahead of time
;;; that there are no circularities and makes expansion much quicker.
(defun intern-search-list (name)
  (let ((name (string-downcase name)))
    (or (gethash name *search-lists*)
	(let ((new (make-search-list :name name)))
	  (setf (gethash name *search-lists*) new)
	  new))))

;;; Clear the definition. Note: we can't remove it from the hash-table
;;; because there may be pathnames still refering to it. So we just
;;; clear out the expansions and ste defined to NIL.
(defun clear-search-list (name)
  #!+sb-doc
  "Clear the current definition for the search-list NAME. Returns T if such
   a definition existed, and NIL if not."
  (let* ((name (string-downcase name))
	 (search-list (gethash name *search-lists*)))
    (when (and search-list (search-list-defined search-list))
      (setf (search-list-defined search-list) nil)
      (setf (search-list-expansions search-list) nil)
      t)))

;;; As in CLEAR-SEARCH-LIST, we can't actually remove the entries from
;;; the hash-table, so we just mark them as being undefined.
(defun clear-all-search-lists ()
  #!+sb-doc
  "Clear the definition for all search-lists. Only use this if you know
   what you are doing."
  (maphash #'(lambda (name search-list)
	       (declare (ignore name))
	       (setf (search-list-defined search-list) nil)
	       (setf (search-list-expansions search-list) nil))
	   *search-lists*)
  nil)

;;; Extract the search-list from PATHNAME and return it. If PATHNAME
;;; doesn't start with a search-list, then either error (if
;;; FLAME-IF-NONE is true) or return NIL (if FLAME-IF-NONE is false).
(defun extract-search-list (pathname flame-if-none)
  (with-pathname (pathname pathname)
    (let* ((directory (%pathname-directory pathname))
	   (search-list (cadr directory)))
      (cond ((search-list-p search-list)
	     search-list)
	    (flame-if-none
	     (error "~S doesn't start with a search-list." pathname))
	    (t
	     nil)))))

;;; We have to convert the internal form of the search-list back into
;;; a bunch of pathnames.
(defun search-list (pathname)
  #!+sb-doc
  "Return the expansions for the search-list starting PATHNAME. If PATHNAME
   does not start with a search-list, then an error is signaled. If
   the search-list has not been defined yet, then an error is signaled.
   The expansion for a search-list can be set with SETF."
  (with-pathname (pathname pathname)
    (let ((search-list (extract-search-list pathname t))
	  (host (pathname-host pathname)))
      (if (search-list-defined search-list)
	  (mapcar #'(lambda (directory)
		      (make-pathname :host host
				     :directory (cons :absolute directory)))
		  (search-list-expansions search-list))
	  (error "Search list ~S has not been defined yet." pathname)))))

(defun search-list-defined-p (pathname)
  #!+sb-doc
  "Returns T if the search-list starting PATHNAME is currently defined, and
   NIL otherwise. An error is signaled if PATHNAME does not start with a
   search-list."
  (with-pathname (pathname pathname)
    (search-list-defined (extract-search-list pathname t))))

;;; Set the expansion for the search list in PATHNAME. If this would
;;; result in any circularities, we flame out. If anything goes wrong,
;;; we leave the old definition intact.
(defun %set-search-list (pathname values)
  (let ((search-list (extract-search-list pathname t)))
    (labels
	((check (target-list path)
	   (when (eq search-list target-list)
	     (error "That would result in a circularity:~%  ~
		     ~A~{ -> ~A~} -> ~A"
		    (search-list-name search-list)
		    (reverse path)
		    (search-list-name target-list)))
	   (when (search-list-p target-list)
	     (push (search-list-name target-list) path)
	     (dolist (expansion (search-list-expansions target-list))
	       (check (car expansion) path))))
	 (convert (pathname)
	   (with-pathname (pathname pathname)
	     (when (or (pathname-name pathname)
		       (pathname-type pathname)
		       (pathname-version pathname))
	       (error "Search-lists cannot expand into pathnames that have ~
		       a name, type, or ~%version specified:~%  ~S"
		      pathname))
	     (let ((directory (pathname-directory pathname)))
	       (let ((expansion
		      (if directory
			  (ecase (car directory)
			    (:absolute (cdr directory))
			    (:relative (cons (intern-search-list "default")
					     (cdr directory))))
			  (list (intern-search-list "default")))))
		 (check (car expansion) nil)
		 expansion)))))
      (setf (search-list-expansions search-list)
	    (if (listp values)
	      (mapcar #'convert values)
	      (list (convert values)))))
    (setf (search-list-defined search-list) t))
  values)

(defun %enumerate-search-list (pathname function)
  (/show0 "entering %ENUMERATE-SEARCH-LIST")
  (let* ((pathname (if (typep pathname 'logical-pathname)
		       (translate-logical-pathname pathname)
		       pathname))
	 (search-list (extract-search-list pathname nil)))
    (/show0 "PATHNAME and SEARCH-LIST computed")
    (cond
     ((not search-list)
      (/show0 "no search list")
      (funcall function pathname))
     ((not (search-list-defined search-list))
      (/show0 "undefined search list")
      (error "undefined search list: ~A"
	     (search-list-name search-list)))
     (t
      (/show0 "general case")
      (let ((tail (cddr (pathname-directory pathname))))
	(/show0 "TAIL computed")
	(dolist (expansion
		 (search-list-expansions search-list))
	  (/show0 "tail recursing in %ENUMERATE-SEARCH-LIST")
	  (%enumerate-search-list (make-pathname :defaults pathname
						 :directory
						 (cons :absolute
						       (append expansion
							       tail)))
				  function)))))))

;;;;  logical pathname support. ANSI 92-102 specification.
;;;;
;;;;  As logical-pathname translations are loaded they are
;;;;  canonicalized as patterns to enable rapid efficent translation
;;;;  into physical pathnames.

;;;; utilities

;;; Canonicalize a logical pathanme word by uppercasing it checking that it
;;; contains only legal characters.
(defun logical-word-or-lose (word)
  (declare (string word))
  (let ((word (string-upcase word)))
    (dotimes (i (length word))
      (let ((ch (schar word i)))
	(unless (or (alpha-char-p ch) (digit-char-p ch) (char= ch #\-))
	  (error 'namestring-parse-error
		 :complaint "logical namestring character which ~
			     is not alphanumeric or hyphen:~%  ~S"
		 :arguments (list ch)
		 :namestring word :offset i))))
    word))

;;; Given a logical host or string, return a logical host. If ERROR-P
;;; is NIL, then return NIL when no such host exists.
(defun find-logical-host (thing &optional (errorp t))
  (etypecase thing
    (string
     (let ((found (gethash (logical-word-or-lose thing)
			   *logical-hosts*)))
       (if (or found (not errorp))
	   found
	   ;; This is the error signalled from e.g.
	   ;; LOGICAL-PATHNAME-TRANSLATIONS when host is not a defined
	   ;; host, and ANSI specifies that that's a TYPE-ERROR.
	   (error 'simple-type-error
		  :datum thing
		  ;; God only knows what ANSI expects us to use for
		  ;; the EXPECTED-TYPE here. Maybe this will be OK..
		  :expected-type
		  '(and string (satisfies logical-pathname-translations))
		  :format-control "logical host not yet defined: ~S"
		  :format-arguments (list thing)))))
    (logical-host thing)))

;;; Given a logical host name or host, return a logical host, creating
;;; a new one if necessary.
(defun intern-logical-host (thing)
  (declare (values logical-host))
  (or (find-logical-host thing nil)
      (let* ((name (logical-word-or-lose thing))
	     (new (make-logical-host :name name)))
	(setf (gethash name *logical-hosts*) new)
	new)))

;;;; logical pathname parsing

;;; Deal with multi-char wildcards in a logical pathname token.
(defun maybe-make-logical-pattern (namestring chunks)
  (let ((chunk (caar chunks)))
    (collect ((pattern))
      (let ((last-pos 0)
	    (len (length chunk)))
	(declare (fixnum last-pos))
	(loop
	  (when (= last-pos len) (return))
	  (let ((pos (or (position #\* chunk :start last-pos) len)))
	    (if (= pos last-pos)
		(when (pattern)
		  (error 'namestring-parse-error
			 :complaint "double asterisk inside of logical ~
				     word: ~S"
			 :arguments (list chunk)
			 :namestring namestring
			 :offset (+ (cdar chunks) pos)))
		(pattern (subseq chunk last-pos pos)))
	    (if (= pos len)
		(return)
		(pattern :multi-char-wild))
	    (setq last-pos (1+ pos)))))
	(aver (pattern))
	(if (cdr (pattern))
	    (make-pattern (pattern))
	    (let ((x (car (pattern))))
	      (if (eq x :multi-char-wild)
		  :wild
		  x))))))

;;; Return a list of conses where the CDR is the start position and
;;; the CAR is a string (token) or character (punctuation.)
(defun logical-chunkify (namestr start end)
  (collect ((chunks))
    (do ((i start (1+ i))
	 (prev 0))
	((= i end)
	 (when (> end prev)
	    (chunks (cons (nstring-upcase (subseq namestr prev end)) prev))))
      (let ((ch (schar namestr i)))
	(unless (or (alpha-char-p ch) (digit-char-p ch)
		    (member ch '(#\- #\*)))
	  (when (> i prev)
	    (chunks (cons (nstring-upcase (subseq namestr prev i)) prev)))
	  (setq prev (1+ i))
	  (unless (member ch '(#\; #\: #\.))
	    (error 'namestring-parse-error
		   :complaint "illegal character for logical pathname:~%  ~S"
		   :arguments (list ch)
		   :namestring namestr
		   :offset i))
	  (chunks (cons ch i)))))
    (chunks)))

;;; Break up a logical-namestring, always a string, into its
;;; constituent parts.
(defun parse-logical-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (collect ((directory))
    (let ((host nil)
	  (name nil)
	  (type nil)
	  (version nil))
      (labels ((expecting (what chunks)
		 (unless (and chunks (simple-string-p (caar chunks)))
		   (error 'namestring-parse-error
			  :complaint "expecting ~A, got ~:[nothing~;~S~]."
			  :arguments (list what (caar chunks) (caar chunks))
			  :namestring namestr
			  :offset (if chunks (cdar chunks) end)))
		 (caar chunks))
	       (parse-host (chunks)
		 (case (caadr chunks)
		   (#\:
		    (setq host
			  (find-logical-host (expecting "a host name" chunks)))
		    (parse-relative (cddr chunks)))
		   (t
		    (parse-relative chunks))))
	       (parse-relative (chunks)
		 (case (caar chunks)
		   (#\;
		    (directory :relative)
		    (parse-directory (cdr chunks)))
		   (t
		    (directory :absolute) ; Assumption! Maybe revoked later.
		    (parse-directory chunks))))
	       (parse-directory (chunks)
		 (case (caadr chunks)
		   (#\;
		    (directory
		     (let ((res (expecting "a directory name" chunks)))
		       (cond ((string= res "..") :up)
			     ((string= res "**") :wild-inferiors)
			     (t
			      (maybe-make-logical-pattern namestr chunks)))))
		    (parse-directory (cddr chunks)))
		   (t
		    (parse-name chunks))))
	       (parse-name (chunks)
		 (when chunks
		   (expecting "a file name" chunks)
		   (setq name (maybe-make-logical-pattern namestr chunks))
		   (expecting-dot (cdr chunks))))
	       (expecting-dot (chunks)
		 (when chunks
		   (unless (eql (caar chunks) #\.)
		     (error 'namestring-parse-error
			    :complaint "expecting a dot, got ~S."
			    :arguments (list (caar chunks))
			    :namestring namestr
			    :offset (cdar chunks)))
		   (if type
		       (parse-version (cdr chunks))
		       (parse-type (cdr chunks)))))
	       (parse-type (chunks)
		 (expecting "a file type" chunks)
		 (setq type (maybe-make-logical-pattern namestr chunks))
		 (expecting-dot (cdr chunks)))
	       (parse-version (chunks)
		 (let ((str (expecting "a positive integer, * or NEWEST"
				       chunks)))
		   (cond
		    ((string= str "*") (setq version :wild))
		    ((string= str "NEWEST") (setq version :newest))
		    (t
		     (multiple-value-bind (res pos)
			 (parse-integer str :junk-allowed t)
		       (unless (and res (plusp res))
			 (error 'namestring-parse-error
				:complaint "expected a positive integer, ~
					    got ~S"
				:arguments (list str)
				:namestring namestr
				:offset (+ pos (cdar chunks))))
		       (setq version res)))))
		 (when (cdr chunks)
		   (error 'namestring-parse-error
			  :complaint "extra stuff after end of file name"
			  :namestring namestr
			  :offset (cdadr chunks)))))
	(parse-host (logical-chunkify namestr start end)))
      (values host :unspecific
	      (and (not (equal (directory)'(:absolute)))(directory))
	      name type version))))

;;; We can't initialize this yet because not all host methods are loaded yet.
(defvar *logical-pathname-defaults*)

(defun logical-pathname (pathspec)
  #!+sb-doc
  "Converts the pathspec argument to a logical-pathname and returns it."
  (declare (type (or logical-pathname string stream) pathspec)
	   (values logical-pathname))
  (if (typep pathspec 'logical-pathname)
      pathspec
      (let ((res (parse-namestring pathspec nil *logical-pathname-defaults*)))
	(when (eq (%pathname-host res)
		  (%pathname-host *logical-pathname-defaults*))
	  (error "This logical namestring does not specify a host:~%  ~S"
		 pathspec))
	res)))

;;;; logical pathname unparsing

(defun unparse-logical-directory (pathname)
  (declare (type pathname pathname))
  (collect ((pieces))
    (let ((directory (%pathname-directory pathname)))
      (when directory
	(ecase (pop directory)
	  (:absolute) ; nothing special
	  (:relative (pieces ";")))
	(dolist (dir directory)
	  (cond ((or (stringp dir) (pattern-p dir))
		 (pieces (unparse-logical-piece dir))
		 (pieces ";"))
		((eq dir :wild)
		 (pieces "*;"))
		((eq dir :wild-inferiors)
		 (pieces "**;"))
		(t
		 (error "invalid directory component: ~S" dir))))))
    (apply #'concatenate 'simple-string (pieces))))

(defun unparse-logical-piece (thing)
  (etypecase thing
    (simple-string thing)
    (pattern
     (collect ((strings))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string (strings piece))
	   (keyword
	    (cond ((eq piece :wild-inferiors)
		   (strings "**"))
		  ((eq piece :multi-char-wild)
		   (strings "*"))
		  (t (error "invalid keyword: ~S" piece))))))
       (apply #'concatenate 'simple-string (strings))))))

;;; Unparse a logical pathname string.
(defun unparse-enough-namestring (pathname defaults)
  (let* ((path-dir (pathname-directory pathname))
         (def-dir (pathname-directory defaults))
         (enough-dir
           ;; Go down the directory lists to see what matches.  What's
           ;; left is what we want, more or less.
           (cond ((and (eq (first path-dir) (first def-dir))
                       (eq (first path-dir) :absolute))
                   ;; Both paths are :ABSOLUTE, so find where the
                   ;; common parts end and return what's left
                   (do* ((p (rest path-dir) (rest p))
                         (d (rest def-dir) (rest d)))
                        ((or (endp p) (endp d)
                             (not (equal (first p) (first d))))
                         `(:relative ,@p))))
                 (t
                   ;; At least one path is :RELATIVE, so just return the
                   ;; original path.  If the original path is :RELATIVE,
                   ;; then that's the right one.  If PATH-DIR is
                   ;; :ABSOLUTE, we want to return that except when
                   ;; DEF-DIR is :ABSOLUTE, as handled above. so return
                   ;; the original directory.
                   path-dir))))
    (make-pathname :host (pathname-host pathname)
                   :directory enough-dir
                   :name (pathname-name pathname)
                   :type (pathname-type pathname)
                   :version (pathname-version pathname))))

(defun unparse-logical-namestring (pathname)
  (declare (type logical-pathname pathname))
  (concatenate 'simple-string
	       (logical-host-name (%pathname-host pathname)) ":"
	       (unparse-logical-directory pathname)
	       (unparse-unix-file pathname)))

;;;; logical pathname translations

;;; Verify that the list of translations consists of lists and prepare
;;; canonical translations. (Parse pathnames and expand out wildcards
;;; into patterns.)
(defun canonicalize-logical-pathname-translations (translation-list host)
  (declare (type list translation-list) (type host host)
	   (values list))
  (mapcar (lambda (translation)
	    (destructuring-bind (from to) translation
	      (list (if (typep from 'logical-pathname)
			from
			(parse-namestring from host))
		    (pathname to)))) 
	  translation-list))

(defun logical-pathname-translations (host)
  #!+sb-doc
  "Return the (logical) host object argument's list of translations."
  (declare (type (or string logical-host) host)
	   (values list))
  (logical-host-translations (find-logical-host host)))

(defun (setf logical-pathname-translations) (translations host)
  #!+sb-doc
  "Set the translations list for the logical host argument.
   Return translations."
  (declare (type (or string logical-host) host)
	   (type list translations)
	   (values list))
  (let ((host (intern-logical-host host)))
    (setf (logical-host-canon-transls host)
	  (canonicalize-logical-pathname-translations translations host))
    (setf (logical-host-translations host) translations)))

(defun translate-logical-pathname (pathname &key)
  #!+sb-doc
  "Translates pathname to a physical pathname, which is returned."
  (declare (type pathname-designator pathname)
	   (values (or null pathname)))
  (typecase pathname
    (logical-pathname
     (dolist (x (logical-host-canon-transls (%pathname-host pathname))
		(error 'simple-file-error
		       :pathname pathname
		       :format-control "no translation for ~S"
		       :format-arguments (list pathname)))
       (destructuring-bind (from to) x
	 (when (pathname-match-p pathname from)
	   (return (translate-logical-pathname
		    (translate-pathname pathname from to)))))))
    (pathname pathname)
    (stream (translate-logical-pathname (pathname pathname)))
    (t (translate-logical-pathname (logical-pathname pathname)))))

(defvar *logical-pathname-defaults*
  (%make-logical-pathname (make-logical-host :name "BOGUS")
			  :unspecific
			  nil
			  nil
			  nil
			  nil))

(defun load-logical-pathname-translations (host)
  #!+sb-doc
  (declare (type string host)
	   (values (member t nil)))
  (if (find-logical-host host nil)
      ;; This host is already defined, all is well and good.
      t
      ;; ANSI: "The specific nature of the search is
      ;; implementation-defined." SBCL: doesn't search at all
      (error "logical host ~S not found" host)))
