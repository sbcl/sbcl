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

;;; To be initialized in unix/win32-pathname.lisp
(defvar *physical-host*)

;; Though it looks like *PHYSICAL-HOST* is unbound here, this form is evaluated
;; much later, by which time the variable has a value.
(def!method make-load-form ((host (eql *physical-host*)) &optional env)
  (declare (ignore env))
  '*physical-host*)

;;; Return a value suitable, e.g., for preinitializing
;;; *DEFAULT-PATHNAME-DEFAULTS* before *DEFAULT-PATHNAME-DEFAULTS* is
;;; initialized (at which time we can't safely call e.g. #'PATHNAME).
(defun make-trivial-default-pathname ()
  (%make-pathname *physical-host* nil nil nil nil :newest))

;;; pathname methods

(def!method print-object ((pathname pathname) stream)
  (let ((namestring (handler-case (namestring pathname)
                      (error nil))))
    (if namestring
        (format stream
                (if (or *print-readably* *print-escape*)
                    "#P~S"
                    "~A")
                (coerce namestring '(simple-array character (*))))
        (print-unreadable-object (pathname stream :type t)
          (format stream
                  "~@<(with no namestring) ~_:HOST ~S ~_:DEVICE ~S ~_:DIRECTORY ~S ~
                  ~_:NAME ~S ~_:TYPE ~S ~_:VERSION ~S~:>"
                  (%pathname-host pathname)
                  (%pathname-device pathname)
                  (%pathname-directory pathname)
                  (%pathname-name pathname)
                  (%pathname-type pathname)
                  (%pathname-version pathname))))))

(def!method make-load-form ((pathname pathname) &optional environment)
  (make-load-form-saving-slots pathname :environment environment))

;;; A pathname is logical if the host component is a logical host.
;;; This constructor is used to make an instance of the correct type
;;; from parsed arguments.
(defun %make-maybe-logical-pathname (host device directory name type version)
  ;; We canonicalize logical pathname components to uppercase. ANSI
  ;; doesn't strictly require this, leaving it up to the implementor;
  ;; but the arguments given in the X3J13 cleanup issue
  ;; PATHNAME-LOGICAL:ADD seem compelling: we should canonicalize the
  ;; case, and uppercase is the ordinary way to do that.
  (flet ((upcase-maybe (x) (typecase x (string (logical-word-or-lose x)) (t x))))
    (if (typep host 'logical-host)
        (%make-logical-pathname host
                                :unspecific
                                (mapcar #'upcase-maybe directory)
                                (upcase-maybe name)
                                (upcase-maybe type)
                                version)
        (progn
          (aver (eq host *physical-host*))
          (%make-pathname host device directory name type version)))))

;;; Hash table searching maps a logical pathname's host to its
;;; physical pathname translation.
(defvar *logical-hosts* (make-hash-table :test 'equal :synchronized t))

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
         (every (lambda (piece1 piece2)
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

;;; If the string matches the pattern returns the multiple values T
;;; and a list of the matched strings.
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
        (simple-string
         ;; String is matched by itself, a matching pattern or :WILD.
         (typecase wild
           (pattern
            (values (pattern-matches wild thing)))
           (simple-string
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
  (or (eq pathname1 pathname2)
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
           (or (eq (%pathname-host pathname1) *physical-host*)
               (compare-component (%pathname-version pathname1)
                                  (%pathname-version pathname2))))))

;;; Convert PATHNAME-DESIGNATOR (a pathname, or string, or
;;; stream), into a pathname in pathname.
;;;
;;; FIXME: was rewritten, should be tested (or rewritten again, this
;;; time using ONCE-ONLY, *then* tested)
(eval-when (:compile-toplevel :execute)
(sb!xc:defmacro with-pathname ((pathname pathname-designator) &body body)
  (let ((pd0 (gensym)))
    `(let* ((,pd0 ,pathname-designator)
            (,pathname (etypecase ,pd0
                         (pathname ,pd0)
                         (string (parse-namestring ,pd0))
                         (file-stream (file-name ,pd0)))))
       ,@body)))

(sb!xc:defmacro with-native-pathname ((pathname pathname-designator) &body body)
  (let ((pd0 (gensym)))
    `(let* ((,pd0 ,pathname-designator)
            (,pathname (etypecase ,pd0
                         (pathname ,pd0)
                         (string (parse-native-namestring ,pd0))
                         ;; FIXME
                         #+nil
                         (file-stream (file-name ,pd0)))))
       ,@body)))

(sb!xc:defmacro with-host ((host host-designator) &body body)
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
  (let ((hd0 (gensym)))
    `(let* ((,hd0 ,host-designator)
            (,host (etypecase ,hd0
                     ((string 0)
                      ;; This is a special host. It's not valid as a
                      ;; logical host, so it is a sensible thing to
                      ;; designate the physical host object. So we do
                      ;; that.
                      *physical-host*)
                     (string
                      ;; In general ANSI-compliant Common Lisps, a
                      ;; string might also be a physical pathname
                      ;; host, but ANSI leaves this up to the
                      ;; implementor, and in SBCL we don't do it, so
                      ;; it must be a logical host.
                      (find-logical-host ,hd0))
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
                             ,hd0))
                     (host ,hd0))))
      ,@body)))
) ; EVAL-WHEN

(defun find-host (host-designator &optional (errorp t))
  (with-host (host host-designator)
    (when (and errorp (not host))
      (error "Couldn't find host: ~S" host-designator))
    host))

(defun pathname (pathspec)
  #!+sb-doc
  "Convert PATHSPEC (a pathname designator) into a pathname."
  (declare (type pathname-designator pathspec))
  (with-pathname (pathname pathspec)
    pathname))

(defun native-pathname (pathspec)
  #!+sb-doc
  "Convert PATHSPEC (a pathname designator) into a pathname, assuming
the operating system native pathname conventions."
  (with-native-pathname (pathname pathspec)
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
                               (case (car piece)
                                 (:character-set
                                  (check-for pred (cdr piece))))))
                        (return t))))
                   (list
                    (dolist (x in)
                      (when (check-for pred x)
                        (return t))))
                   (simple-string
                    (dotimes (i (length in))
                      (when (funcall pred (schar in i))
                        (return t))))
                   (t nil)))
               (diddle-with (fun thing)
                 (typecase thing
                   (pattern
                    (make-pattern
                     (mapcar (lambda (piece)
                               (typecase piece
                                 (simple-string
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
                   (simple-string
                    (funcall fun thing))
                   (t
                    thing))))
        (let ((any-uppers (check-for #'upper-case-p thing))
              (any-lowers (check-for #'lower-case-p thing)))
          (cond ((and any-uppers any-lowers)
                 ;; mixed case, stays the same
                 thing)
                (any-uppers
                 ;; all uppercase, becomes all lower case
                 (diddle-with (lambda (x) (if (stringp x)
                                              (string-downcase x)
                                              x)) thing))
                (any-lowers
                 ;; all lowercase, becomes all upper case
                 (diddle-with (lambda (x) (if (stringp x)
                                              (string-upcase x)
                                              x)) thing))
                (t
                 ;; no letters?  I guess just leave it.
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
                          (typep (car results) '(or string pattern
                                                 (member :wild :wild-inferiors))))
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
                            (host-customary-case pathname-host)))))
             (directory (merge-directories (%pathname-directory pathname)
                                           (%pathname-directory defaults)
                                           diddle-case)))
        (%make-maybe-logical-pathname
         (or pathname-host default-host)
         (and ;; The device of ~/ shouldn't be merged,
              ;; because the expansion may have a different device
              (not (and (>= (length directory) 2)
                        (eql (car directory) :absolute)
                        (eql (cadr directory) :home)))
              (or (%pathname-device pathname)
                  (maybe-diddle-case (%pathname-device defaults)
                                     diddle-case)))
         directory
         (or (%pathname-name pathname)
             (maybe-diddle-case (%pathname-name defaults)
                                diddle-case))
         (or (%pathname-type pathname)
             (maybe-diddle-case (%pathname-type defaults)
                                diddle-case))
         (or (%pathname-version pathname)
             (and (not (%pathname-name pathname)) (%pathname-version defaults))
             default-version))))))

(defun import-directory (directory diddle-case)
  (etypecase directory
    (null nil)
    ((member :wild) '(:absolute :wild-inferiors))
    ((member :unspecific) '(:relative))
    (list
     (let ((root (pop directory))
           results)
       (if (member root '(:relative :absolute))
           (push root results)
           (error "List of directory components must start with ~S or ~S."
                  :absolute :relative))
       (when directory
         (let ((next (car directory)))
           (when (or (eq :home next)
                     (typep next '(cons (eql :home) (cons string null))))
             (push (pop directory) results)))
         (dolist (piece directory)
           (typecase piece
             ((member :wild :wild-inferiors :up)
              (push piece results))
             ((member :back)
              (if (typep (car results) '(or string pattern
                                         (member :wild :wild-inferiors)))
                  (pop results)
                  (push piece results)))
             ((or simple-string pattern)
              (push (maybe-diddle-case piece diddle-case) results))
             (string
              (push (maybe-diddle-case (coerce piece 'simple-string)
                                       diddle-case) results))

             (t
              (error "~S is not allowed as a directory component." piece)))))
       (nreverse results)))
    (simple-string
     `(:absolute ,(maybe-diddle-case directory diddle-case)))
    (string
     `(:absolute
       ,(maybe-diddle-case (coerce directory 'simple-string) diddle-case)))))

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
  (declare (type (or string host pathname-component-tokens) host)
           (type (or string pathname-component-tokens) device)
           (type (or list string pattern pathname-component-tokens) directory)
           (type (or string pattern pathname-component-tokens) name type)
           (type (or integer pathname-component-tokens (member :newest))
                 version)
           (type (or pathname-designator null) defaults)
           (type (member :common :local) case))
  (let* ((defaults (when defaults
                     (with-pathname (defaults defaults) defaults)))
         (default-host (if defaults
                           (%pathname-host defaults)
                           (pathname-host *default-pathname-defaults*)))
         ;; Raymond Toy writes: CLHS says make-pathname can take a
         ;; string (as a logical-host) for the host part. We map that
         ;; string into the corresponding logical host structure.
         ;;
         ;; Paul Werkowski writes:
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
         (host (or (find-host host nil) default-host))
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
  "Return PATHNAME's host."
  (declare (type pathname-designator pathname)
           (type (member :local :common) case)
           (values host)
           (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

(defun pathname-device (pathname &key (case :local))
  #!+sb-doc
  "Return PATHNAME's device."
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
  "Return PATHNAME's directory."
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
  "Return PATHNAME's name."
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
  "Return PATHNAME's type."
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
  "Return PATHNAME's version."
  (declare (type pathname-designator pathname))
  (with-pathname (pathname pathname)
    (%pathname-version pathname)))

;;;; namestrings

;;; Handle the case for PARSE-NAMESTRING parsing a potentially
;;; syntactically valid logical namestring with an explicit host.
;;;
;;; This then isn't fully general -- we are relying on the fact that
;;; we will only pass to parse-namestring namestring with an explicit
;;; logical host, so that we can pass the host return from
;;; parse-logical-namestring through to %PARSE-NAMESTRING as a truth
;;; value. Yeah, this is probably a KLUDGE - CSR, 2002-04-18
(defun parseable-logical-namestring-p (namestr start end)
  (catch 'exit
    (handler-bind
        ((namestring-parse-error (lambda (c)
                                   (declare (ignore c))
                                   (throw 'exit nil))))
      (let ((colon (position #\: namestr :start start :end end)))
        (when colon
          (let ((potential-host
                 (logical-word-or-lose (subseq namestr start colon))))
            ;; depending on the outcome of CSR comp.lang.lisp post
            ;; "can PARSE-NAMESTRING create logical hosts", we may need
            ;; to do things with potential-host (create it
            ;; temporarily, parse the namestring and unintern the
            ;; logical host potential-host on failure.
            (declare (ignore potential-host))
            (let ((result
                   (handler-bind
                       ((simple-type-error (lambda (c)
                                             (declare (ignore c))
                                             (throw 'exit nil))))
                     (parse-logical-namestring namestr start end))))
              ;; if we got this far, we should have an explicit host
              ;; (first return value of parse-logical-namestring)
              (aver result)
              result)))))))

;;; Handle the case where PARSE-NAMESTRING is actually parsing a
;;; namestring. We pick off the :JUNK-ALLOWED case then find a host to
;;; use for parsing, call the parser, then check whether the host matches.
(defun %parse-namestring (namestr host defaults start end junk-allowed)
  (declare (type (or host null) host)
           (type string namestr)
           (type index start)
           (type (or index null) end))
  (cond
    (junk-allowed
     (handler-case
         (%parse-namestring namestr host defaults start end nil)
       (namestring-parse-error (condition)
         (values nil (namestring-parse-error-offset condition)))))
    (t
     (let* ((end (%check-vector-sequence-bounds namestr start end)))
       (multiple-value-bind (new-host device directory file type version)
           ;; Comments below are quotes from the HyperSpec
           ;; PARSE-NAMESTRING entry, reproduced here to demonstrate
           ;; that we actually have to do things this way rather than
           ;; some possibly more logical way. - CSR, 2002-04-18
           (cond
             ;; "If host is a logical host then thing is parsed as a
             ;; logical pathname namestring on the host."
             (host (funcall (host-parse host) namestr start end))
             ;; "If host is nil and thing is a syntactically valid
             ;; logical pathname namestring containing an explicit
             ;; host, then it is parsed as a logical pathname
             ;; namestring."
             ((parseable-logical-namestring-p namestr start end)
              (parse-logical-namestring namestr start end))
             ;; "If host is nil, default-pathname is a logical
             ;; pathname, and thing is a syntactically valid logical
             ;; pathname namestring without an explicit host, then it
             ;; is parsed as a logical pathname namestring on the
             ;; host that is the host component of default-pathname."
             ;;
             ;; "Otherwise, the parsing of thing is
             ;; implementation-defined."
             ;;
             ;; Both clauses are handled here, as the default
             ;; *DEFAULT-PATHNAME-DEFAULTS* has a SB-IMPL::UNIX-HOST
             ;; for a host.
             ((pathname-host defaults)
              (funcall (host-parse (pathname-host defaults))
                       namestr
                       start
                       end))
             ;; I don't think we should ever get here, as the default
             ;; host will always have a non-null HOST, given that we
             ;; can't create a new pathname without going through
             ;; *DEFAULT-PATHNAME-DEFAULTS*, which has a non-null
             ;; host...
             (t (bug "Fallen through COND in %PARSE-NAMESTRING")))
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
         (let ((pn-host (or new-host host (pathname-host defaults))))
           (values (%make-maybe-logical-pathname
                    pn-host device directory file type version)
                   end)))))))

;;; If NAMESTR begins with a colon-terminated, defined, logical host,
;;; then return that host, otherwise return NIL.
(defun extract-logical-host-prefix (namestr start end)
  (declare (type simple-string namestr)
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
  (declare (type pathname-designator thing defaults)
           (type (or list host string (member :unspecific)) host)
           (type index start)
           (type (or index null) end)
           (type (or t null) junk-allowed)
           (values (or null pathname) (or null index)))
  (with-host (found-host host)
    (let (;; According to ANSI defaults may be any valid pathname designator
          (defaults (etypecase defaults
                      (pathname
                       defaults)
                      (string
                       (aver (pathnamep *default-pathname-defaults*))
                       (parse-namestring defaults))
                      (stream
                       (truename defaults)))))
      (declare (type pathname defaults))
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
           (values name nil)))))))

(defun %parse-native-namestring (namestr host defaults start end junk-allowed
                                 as-directory)
  (declare (type (or host null) host)
           (type string namestr)
           (type index start)
           (type (or index null) end))
  (cond
    (junk-allowed
     (handler-case
         (%parse-native-namestring namestr host defaults start end nil as-directory)
       (namestring-parse-error (condition)
         (values nil (namestring-parse-error-offset condition)))))
    (t
     (let* ((end (%check-vector-sequence-bounds namestr start end)))
       (multiple-value-bind (new-host device directory file type version)
           (cond
             (host
              (funcall (host-parse-native host) namestr start end as-directory))
             ((pathname-host defaults)
              (funcall (host-parse-native (pathname-host defaults))
                       namestr
                       start
                       end
                       as-directory))
             ;; I don't think we should ever get here, as the default
             ;; host will always have a non-null HOST, given that we
             ;; can't create a new pathname without going through
             ;; *DEFAULT-PATHNAME-DEFAULTS*, which has a non-null
             ;; host...
             (t (bug "Fallen through COND in %PARSE-NAMESTRING")))
         (when (and host new-host (not (eq new-host host)))
           (error 'simple-type-error
                  :datum new-host
                  :expected-type `(or null (eql ,host))
                  :format-control
                  "The host in the namestring, ~S,~@
                   does not match the explicit HOST argument, ~S."
                  :format-arguments (list new-host host)))
         (let ((pn-host (or new-host host (pathname-host defaults))))
           (values (%make-pathname
                    pn-host device directory file type version)
                   end)))))))

(defun parse-native-namestring (thing
                                &optional
                                host
                                (defaults *default-pathname-defaults*)
                                &key (start 0) end junk-allowed
                                as-directory)
  #!+sb-doc
  "Convert THING into a pathname, using the native conventions
appropriate for the pathname host HOST, or if not specified the
host of DEFAULTS.  If THING is a string, the parse is bounded by
START and END, and error behaviour is controlled by JUNK-ALLOWED,
as with PARSE-NAMESTRING.  For file systems whose native
conventions allow directories to be indicated as files, if
AS-DIRECTORY is true, return a pathname denoting THING as a
directory."
  (declare (type pathname-designator thing defaults)
           (type (or list host string (member :unspecific)) host)
           (type index start)
           (type (or index null) end)
           (type (or t null) junk-allowed)
           (values (or null pathname) (or null index)))
  (with-host (found-host host)
    (let ((defaults (etypecase defaults
                      (pathname
                       defaults)
                      (string
                       (aver (pathnamep *default-pathname-defaults*))
                       (parse-native-namestring defaults))
                      (stream
                       (truename defaults)))))
      (declare (type pathname defaults))
      (etypecase thing
        (simple-string
         (%parse-native-namestring
          thing found-host defaults start end junk-allowed as-directory))
        (string
         (%parse-native-namestring (coerce thing 'simple-string)
                                   found-host defaults start end junk-allowed
                                   as-directory))
        (pathname
         (let ((defaulted-host (or found-host (%pathname-host defaults))))
           (declare (type host defaulted-host))
           (unless (eq defaulted-host (%pathname-host thing))
             (error "The HOST argument doesn't match the pathname host:~%  ~
                     ~S and ~S."
                    defaulted-host (%pathname-host thing))))
         (values thing start))
        (stream
         ;; FIXME
         (let ((name (file-name thing)))
           (unless name
             (error "can't figure out the file associated with stream:~%  ~S"
                    thing))
           (values name nil)))))))

(defun namestring (pathname)
  #!+sb-doc
  "Construct the full (name)string form of the pathname."
  (declare (type pathname-designator pathname))
  (with-pathname (pathname pathname)
    (when pathname
      (let ((host (%pathname-host pathname)))
        (unless host
          (error "can't determine the namestring for pathnames with no ~
                  host:~%  ~S" pathname))
        (funcall (host-unparse host) pathname)))))

(defun native-namestring (pathname &key as-file)
  #!+sb-doc
  "Construct the full native (name)string form of PATHNAME.  For
file systems whose native conventions allow directories to be
indicated as files, if AS-FILE is true and the name, type, and
version components of PATHNAME are all NIL or :UNSPECIFIC,
construct a string that names the directory according to the file
system's syntax for files."
  (declare (type pathname-designator pathname))
  (with-native-pathname (pathname pathname)
    (when pathname
      (let ((host (%pathname-host pathname)))
        (unless host
          (error "can't determine the native namestring for pathnames with no ~
                  host:~%  ~S" pathname))
        (funcall (host-unparse-native host) pathname as-file)))))

(defun host-namestring (pathname)
  #!+sb-doc
  "Return a string representation of the name of the host in the pathname."
  (declare (type pathname-designator pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
          (funcall (host-unparse-host host) pathname)
          (error
           "can't determine the namestring for pathnames with no host:~%  ~S"
           pathname)))))

(defun directory-namestring (pathname)
  #!+sb-doc
  "Return a string representation of the directories used in the pathname."
  (declare (type pathname-designator pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
          (funcall (host-unparse-directory host) pathname)
          (error
           "can't determine the namestring for pathnames with no host:~%  ~S"
           pathname)))))

(defun file-namestring (pathname)
  #!+sb-doc
  "Return a string representation of the name used in the pathname."
  (declare (type pathname-designator pathname))
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
  "Return an abbreviated pathname sufficient to identify the pathname relative
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
      (macrolet ((frob (field &optional (op 'components-match))
                   `(or (null (,field wildname))
                        (,op (,field pathname) (,field wildname)))))
        (and (or (null (%pathname-host wildname))
                 (eq (%pathname-host wildname) (%pathname-host pathname)))
             (frob %pathname-device)
             (frob %pathname-directory directory-components-match)
             (frob %pathname-name)
             (frob %pathname-type)
             (or (eq (%pathname-host wildname) *physical-host*)
                 (frob %pathname-version)))))))

;;; Place the substitutions into the pattern and return the string or pattern
;;; that results. If DIDDLE-CASE is true, we diddle the result case as well,
;;; in case we are translating between hosts with difference conventional case.
;;; The second value is the tail of subs with all of the values that we used up
;;; stripped off. Note that PATTERN-MATCHES matches all consecutive wildcards
;;; as a single string, so we ignore subsequent contiguous wildcards.
(defun substitute-into (pattern subs diddle-case)
  (declare (type pattern pattern)
           (type list subs)
           (values (or simple-string pattern) list))
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

;;; Do TRANSLATE-COMPONENT for all components except host, directory
;;; and version.
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
          (unless (every (lambda (x) (eq x :wild-inferiors)) from)
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
                 (from-host (%pathname-host from))
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
               (if (eq from-host *physical-host*)
                   (if (or (eq (%pathname-version to) :wild)
                           (eq (%pathname-version to) nil))
                       (%pathname-version source)
                       (%pathname-version to))
                   (frob %pathname-version)))))))))

;;;;  logical pathname support. ANSI 92-102 specification.
;;;;
;;;;  As logical-pathname translations are loaded they are
;;;;  canonicalized as patterns to enable rapid efficient translation
;;;;  into physical pathnames.

;;;; utilities

(defun simplify-namestring (namestring &optional host)
  (funcall (host-simplify-namestring
            (or host
                (pathname-host (sane-default-pathname-defaults))))
           namestring))

;;; Canonicalize a logical pathname word by uppercasing it checking that it
;;; contains only legal characters.
(defun logical-word-or-lose (word)
  (declare (string word))
  (when (string= word "")
    (error 'namestring-parse-error
           :complaint "Attempted to treat invalid logical hostname ~
                       as a logical host:~%  ~S"
           :args (list word)
           :namestring word :offset 0))
  (let ((word (string-upcase word)))
    (dotimes (i (length word))
      (let ((ch (schar word i)))
        (unless (and (typep ch 'standard-char)
                     (or (alpha-char-p ch) (digit-char-p ch) (char= ch #\-)))
          (error 'namestring-parse-error
                 :complaint "logical namestring character which ~
                             is not alphanumeric or hyphen:~%  ~S"
                 :args (list ch)
                 :namestring word :offset i))))
    (coerce word 'string))) ; why not simple-string?

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
  (with-locked-system-table (*logical-hosts*)
    (or (find-logical-host thing nil)
        (let* ((name (logical-word-or-lose thing))
               (new (make-logical-host :name name)))
          (setf (gethash name *logical-hosts*) new)
          new))))

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
                         :args (list chunk)
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
                   :args (list ch)
                   :namestring namestr
                   :offset i))
          (chunks (cons ch i)))))
    (chunks)))

;;; Break up a logical-namestring, always a string, into its
;;; constituent parts.
(defun parse-logical-namestring (namestr start end)
  (declare (type simple-string namestr)
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
                          :args (list what (caar chunks) (caar chunks))
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
                            :args (list (caar chunks))
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
                                :args (list str)
                                :namestring namestr
                                :offset (+ pos (cdar chunks))))
                       (setq version res)))))
                 (when (cdr chunks)
                   (error 'namestring-parse-error
                          :complaint "extra stuff after end of file name"
                          :namestring namestr
                          :offset (cdadr chunks)))))
        (parse-host (logical-chunkify namestr start end)))
      (values host :unspecific (directory) name type version))))

;;; We can't initialize this yet because not all host methods are
;;; loaded yet.
(defvar *logical-pathname-defaults*)

(defun logical-namestring-p (x)
  (and (stringp x)
       (ignore-errors
         (typep (pathname x) 'logical-pathname))))

(deftype logical-namestring ()
  `(satisfies logical-namestring-p))

(defun logical-pathname (pathspec)
  #!+sb-doc
  "Converts the pathspec argument to a logical-pathname and returns it."
  (declare (type (or logical-pathname string stream) pathspec)
           (values logical-pathname))
  (if (typep pathspec 'logical-pathname)
      pathspec
      (flet ((oops (problem)
               (error 'simple-type-error
                      :datum pathspec
                      :expected-type 'logical-namestring
                      :format-control "~S is not a valid logical namestring:~%  ~A"
                      :format-arguments (list pathspec problem))))
        (let ((res (handler-case
                       (parse-namestring pathspec nil *logical-pathname-defaults*)
                     (error (e) (oops e)))))
          (when (eq (%pathname-host res)
                    (%pathname-host *logical-pathname-defaults*))
            (oops "no host specified"))
          res))))

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
    ((member :wild) "*")
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

(defun unparse-logical-file (pathname)
  (declare (type pathname pathname))
    (collect ((strings))
    (let* ((name (%pathname-name pathname))
           (type (%pathname-type pathname))
           (version (%pathname-version pathname))
           (type-supplied (not (or (null type) (eq type :unspecific))))
           (version-supplied (not (or (null version)
                                      (eq version :unspecific)))))
      (when name
        (when (and (null type)
                   (typep name 'string)
                   (position #\. name :start 1))
          (error "too many dots in the name: ~S" pathname))
        (strings (unparse-logical-piece name)))
      (when type-supplied
        (unless name
          (error "cannot specify the type without a file: ~S" pathname))
        (when (typep type 'string)
          (when (position #\. type)
            (error "type component can't have a #\. inside: ~S" pathname)))
        (strings ".")
        (strings (unparse-logical-piece type)))
      (when version-supplied
        (unless type-supplied
          (error "cannot specify the version without a type: ~S" pathname))
        (etypecase version
          ((member :newest) (strings ".NEWEST"))
          ((member :wild) (strings ".*"))
          (fixnum (strings ".") (strings (format nil "~D" version))))))
    (apply #'concatenate 'simple-string (strings))))

;;; Unparse a logical pathname string.
(defun unparse-enough-namestring (pathname defaults)
  (let* ((path-directory (pathname-directory pathname))
         (def-directory (pathname-directory defaults))
         (enough-directory
           ;; Go down the directory lists to see what matches.  What's
           ;; left is what we want, more or less.
           (cond ((and (eq (first path-directory) (first def-directory))
                       (eq (first path-directory) :absolute))
                   ;; Both paths are :ABSOLUTE, so find where the
                   ;; common parts end and return what's left
                   (do* ((p (rest path-directory) (rest p))
                         (d (rest def-directory) (rest d)))
                        ((or (endp p) (endp d)
                             (not (equal (first p) (first d))))
                         `(:relative ,@p))))
                 (t
                   ;; At least one path is :RELATIVE, so just return the
                   ;; original path.  If the original path is :RELATIVE,
                   ;; then that's the right one.  If PATH-DIRECTORY is
                   ;; :ABSOLUTE, we want to return that except when
                   ;; DEF-DIRECTORY is :ABSOLUTE, as handled above. so return
                   ;; the original directory.
                   path-directory))))
    (unparse-logical-namestring
     (make-pathname :host (pathname-host pathname)
                    :directory enough-directory
                    :name (pathname-name pathname)
                    :type (pathname-type pathname)
                    :version (pathname-version pathname)))))

(defun unparse-logical-namestring (pathname)
  (declare (type logical-pathname pathname))
  (concatenate 'simple-string
               (logical-host-name (%pathname-host pathname)) ":"
               (unparse-logical-directory pathname)
               (unparse-logical-file pathname)))

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
  "Set the translations list for the logical host argument."
  (declare (type (or string logical-host) host)
           (type list translations)
           (values list))
  (let ((host (intern-logical-host host)))
    (setf (logical-host-canon-transls host)
          (canonicalize-logical-pathname-translations translations host))
    (setf (logical-host-translations host) translations)))

(defun translate-logical-pathname (pathname &key)
  #!+sb-doc
  "Translate PATHNAME to a physical pathname, which is returned."
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
    (t (translate-logical-pathname (pathname pathname)))))

(defvar *logical-pathname-defaults*
  (%make-logical-pathname
   (make-logical-host :name (logical-word-or-lose "BOGUS"))
   :unspecific nil nil nil nil))

(defun load-logical-pathname-translations (host)
  #!+sb-doc
  "Reads logical pathname translations from SYS:SITE;HOST.TRANSLATIONS.NEWEST,
with HOST replaced by the supplied parameter. Returns T on success.

If HOST is already defined as logical pathname host, no file is loaded and NIL
is returned.

The file should contain a single form, suitable for use with
\(SETF LOGICAL-PATHNAME-TRANSLATIONS).

Note: behaviour of this function is highly implementation dependent, and
historically it used to be a no-op in SBCL -- the current approach is somewhat
experimental and subject to change."
  (declare (type string host)
           (values (member t nil)))
  (if (find-logical-host host nil)
      ;; This host is already defined, all is well and good.
      nil
      ;; ANSI: "The specific nature of the search is
      ;; implementation-defined."
      (prog1 t
        (setf (logical-pathname-translations host)
              (with-open-file (lpt (make-pathname :host "SYS"
                                                  :directory '(:absolute "SITE")
                                                  :name host
                                                  :type "TRANSLATIONS"
                                                  :version :newest))
                (read lpt))))))

(defun !pathname-cold-init ()
  (let* ((sys *default-pathname-defaults*)
         (src
          (merge-pathnames
           (make-pathname :directory '(:relative "src" :wild-inferiors)
                          :name :wild :type :wild)
           sys))
         (contrib
          (merge-pathnames
           (make-pathname :directory '(:relative "contrib" :wild-inferiors)
                          :name :wild :type :wild)
           sys))
         (output
          (merge-pathnames
           (make-pathname :directory '(:relative "output" :wild-inferiors)
                          :name :wild :type :wild)
           sys)))
    (setf (logical-pathname-translations "SYS")
          `(("SYS:SRC;**;*.*.*" ,src)
            ("SYS:CONTRIB;**;*.*.*" ,contrib)
            ("SYS:OUTPUT;**;*.*.*" ,output)))))

(defun set-sbcl-source-location (pathname)
  #!+sb-doc
  "Initialize the SYS logical host based on PATHNAME, which should be
the top-level directory of the SBCL sources. This will replace any
existing translations for \"SYS:SRC;\", \"SYS:CONTRIB;\", and
\"SYS:OUTPUT;\". Other \"SYS:\" translations are preserved."
  (let ((truename (truename pathname))
        (current-translations
         (remove-if (lambda (translation)
                      (or (pathname-match-p "SYS:SRC;" translation)
                          (pathname-match-p "SYS:CONTRIB;" translation)
                          (pathname-match-p "SYS:OUTPUT;" translation)))
                    (logical-pathname-translations "SYS")
                    :key #'first)))
    (flet ((physical-target (component)
             (merge-pathnames
              (make-pathname :directory (list :relative component
                                              :wild-inferiors)
                             :name :wild
                             :type :wild)
              truename)))
      (setf (logical-pathname-translations "SYS")
            `(("SYS:SRC;**;*.*.*" ,(physical-target "src"))
              ("SYS:CONTRIB;**;*.*.*" ,(physical-target "contrib"))
              ("SYS:OUTPUT;**;*.*.*" ,(physical-target "output"))
              ,@current-translations)))))
