;;;; file system interface functions -- fairly Unix-centric, but with
;;;; differences between Unix and Win32 papered over.

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

;;; FIXME: the below shouldn't really be here, but in documentation
;;; (chapter 19 makes a lot of requirements for documenting
;;; implementation-dependent decisions), but anyway it's probably not
;;; what we currently do.
;;;
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
;;; - Otherwise, the last dot separates the file and the type.
;;;
;;; Wildcard characters:
;;;
;;; If the directory, file, type components contain any of the
;;; following characters, it is considered part of a wildcard pattern
;;; and has the following meaning.
;;;
;;; ? - matches any one character
;;; * - matches any zero or more characters.
;;; [abc] - matches any of a, b, or c.
;;; {str1,str2,...,strn} - matches any of str1, str2, ..., or strn.
;;;   (FIXME: no it doesn't)
;;;
;;; Any of these special characters can be preceded by a backslash to
;;; cause it to be treated as a regular character.
(defun remove-backslashes (namestr start end)
  #!+sb-doc
  "Remove any occurrences of #\\ from the string because we've already
   checked for whatever they may have protected."
  (declare (type simple-string namestr)
           (type index start end))
  (let* ((result (make-string (- end start) :element-type 'character))
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
    (%shrink-vector result dst)))

(defvar *ignore-wildcards* nil)

(/show0 "filesys.lisp 86")

(defun maybe-make-pattern (namestr start end)
  (declare (type simple-string namestr)
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
                         (pattern (cons :character-set
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
  (declare (type simple-string namestr)
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
;;;
;;; KLUDGE: this assumes that an absolute pathname is indicated to the
;;; operating system by having a directory separator as the first
;;; character in the directory part.  This is true for Win32 pathnames
;;; and for Unix pathnames, but it isn't true for LispM pathnames (and
;;; their bastard offspring, logical pathnames.  Also it assumes that
;;; Unix pathnames have an empty or :unspecific device, and that
;;; windows drive letters are the only kinds of non-empty/:UNSPECIFIC
;;; devices.
(defun %enumerate-matches (pathname verify-existence follow-links function)
  (/noshow0 "entering %ENUMERATE-MATCHES")
  (when (pathname-type pathname)
    (unless (pathname-name pathname)
      (error "cannot supply a type without a name:~%  ~S" pathname)))
  (when (and (integerp (pathname-version pathname))
             (member (pathname-type pathname) '(nil :unspecific)))
    (error "cannot supply a version without a type:~%  ~S" pathname))
  (let ((host (pathname-host pathname))
        (device (pathname-device pathname))
        (directory (pathname-directory pathname)))
    (/noshow0 "computed HOST and DIRECTORY")
    (let* ((dirstring (if directory
                          (ecase (first directory)
                            (:absolute (host-unparse-directory-separator host))
                            (:relative ""))
                          ""))
           (devstring (if (and device (not (eq device :unspecific)))
                          (concatenate 'simple-string (string device) (string #\:))
                          ""))
           (headstring (concatenate 'simple-string devstring dirstring)))
      (if directory
          (%enumerate-directories headstring (rest directory) pathname
                                  verify-existence follow-links nil function)
          (%enumerate-files headstring pathname verify-existence function)))))

;;; Call FUNCTION on directories.
(defun %enumerate-directories (head tail pathname verify-existence
                               follow-links nodes function
                               &aux (host (pathname-host pathname)))
  (declare (simple-string head))
  #!+win32
  (setf follow-links nil)
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
             (let ((head (concatenate 'string head piece)))
               (with-directory-node-noted (head)
                 (%enumerate-directories
                  (concatenate 'string head
                               (host-unparse-directory-separator host))
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
             (dolist (name (directory-lispy-filenames head))
               (let ((subdir (concatenate 'string head name)))
                 (multiple-value-bind (res dev ino mode)
                     (unix-xstat subdir)
                   (declare (type (or fixnum null) mode))
                   (when (and res (eql (logand mode sb!unix:s-ifmt)
                                       sb!unix:s-ifdir))
                     (unless (dolist (dir nodes nil)
                               (when (and (eql (car dir) dev)
                                          #!+win32 ;; KLUDGE
                                          (not (zerop ino))
                                          (eql (cdr dir) ino))
                                 (return t)))
                       (let ((nodes (cons (cons dev ino) nodes))
                             (subdir (concatenate 'string subdir (host-unparse-directory-separator host))))
                         (%enumerate-directories subdir tail pathname
                                                 verify-existence follow-links
                                                 nodes function))))))))
            ((or pattern (member :wild))
             (dolist (name (directory-lispy-filenames head))
               (when (or (eq piece :wild) (pattern-matches piece name))
                 (let ((subdir (concatenate 'string head name)))
                   (multiple-value-bind (res dev ino mode)
                       (unix-xstat subdir)
                     (declare (type (or fixnum null) mode))
                     (when (and res
                                (eql (logand mode sb!unix:s-ifmt)
                                     sb!unix:s-ifdir))
                       (let ((nodes (cons (cons dev ino) nodes))
                             (subdir (concatenate 'string subdir (host-unparse-directory-separator host))))
                         (%enumerate-directories subdir (rest tail) pathname
                                                 verify-existence follow-links
                                                 nodes function))))))))
          ((member :up)
           (when (string= head (host-unparse-directory-separator host))
             (error 'simple-file-error
                    :pathname pathname
                    :format-control "~@<invalid use of :UP after :ABSOLUTE.~@:>"))
           (with-directory-node-removed (head)
             (let ((head (concatenate 'string head "..")))
               (with-directory-node-noted (head)
                 (%enumerate-directories (concatenate 'string head (host-unparse-directory-separator host))
                                         (rest tail) pathname
                                         verify-existence follow-links
                                         nodes function)))))
          ((member :back)
           ;; :WILD-INFERIORS is handled above, so the only case here
           ;; should be (:ABSOLUTE :BACK)
           (aver (string= head (host-unparse-directory-separator host)))
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
           (let ((directory (coerce directory 'string)))
             (when (or (not verify-existence)
                       (sb!unix:unix-file-kind directory))
               (funcall function directory))))
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
                          (concatenate 'string
                                       directory
                                       complete-filename))))))
          (t
           (/noshow0 "default case")
           (let ((file (concatenate 'string directory name)))
             (/noshow "computed basic FILE")
             (unless (or (null type) (eq type :unspecific))
               (/noshow0 "tweaking FILE for more-or-less-:UNSPECIFIC case")
               (setf file (concatenate 'string file "." type)))
             (unless (member version '(nil :newest :wild :unspecific))
               (/noshow0 "tweaking FILE for more-or-less-:WILD case")
               (setf file (concatenate 'string file "."
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
               (%shrink-vector res (- len i)))
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
;;;
;;; FIXME: apart from the error checking (for wildness and for
;;; existence) and conversion to physical pathanme, this is redundant
;;; with UNPARSE-NATIVE-UNIX-NAMESTRING; one should probably be
;;; written in terms of the other.
;;;
;;; FIXME: actually this (I think) works not just for Unix.
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
An error of type FILE-ERROR is signalled if no such file exists, or the
pathname is wild.

Under Unix, the TRUENAME of a broken symlink is considered to be the name of
the broken symlink itself."
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
                 (name (simplify-namestring 
                        trueishname 
                        (pathname-host defaulted-pathname))))
            (if (eq (sb!unix:unix-file-kind name) :directory)
                ;; FIXME: this might work, but it's ugly.
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

(defun ensure-trailing-slash (string)
  (let ((last-char (char string (1- (length string)))))
         (if (or (eql last-char #\/)
                 #!+win32
                 (eql last-char #\\))
             string
             (concatenate 'string string "/"))))

(defun sbcl-homedir-pathname ()
  (let ((sbcl-home (posix-getenv "SBCL_HOME")))
    ;; SBCL_HOME isn't set for :EXECUTABLE T embedded cores
    (when sbcl-home
      (parse-native-namestring
       (ensure-trailing-slash sbcl-home)))))

;;; (This is an ANSI Common Lisp function.)
(defun user-homedir-pathname (&optional host)
  #!+sb-doc
  "Return the home directory of the user as a pathname. If the HOME
environment variable has been specified, the directory it designates
is returned; otherwise obtains the home directory from the operating
system."
  (declare (ignore host))
  (parse-native-namestring
   (ensure-trailing-slash
    (if (posix-getenv "HOME")
        (posix-getenv "HOME")
        #!-win32
        (sb!unix:uid-homedir (sb!unix:unix-getuid))
        #!+win32
        ;; Needs to bypass PARSE-NATIVE-NAMESTRING & ENSURE-TRAILING-SLASH
        (return-from user-homedir-pathname
          (sb!win32::get-folder-pathname sb!win32::csidl_profile))))))

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

;;; NOTE: There is a fair amount of hair below that is probably not
;;; strictly necessary.
;;;
;;; The issue is the following: what does (DIRECTORY "SYS:*;") mean?
;;; Until 2004-01, SBCL's behaviour was unquestionably wrong, as it
;;; did not translate the logical pathname at all, but instead treated
;;; it as a physical one.  Other Lisps seem to to treat this call as
;;; equivalent to (DIRECTORY (TRANSLATE-LOGICAL-PATHNAME "SYS:*;")),
;;; which is fine as far as it goes, but not very interesting, and
;;; arguably counterintuitive.  (PATHNAME-MATCH-P "SYS:SRC;" "SYS:*;")
;;; is true, so why should "SYS:SRC;" not show up in the call to
;;; DIRECTORY?  (assuming the physical pathname corresponding to it
;;; exists, of course).
;;;
;;; So, the interpretation that I am pushing is for all pathnames
;;; matching the input pathname to be queried.  This means that we
;;; need to compute the intersection of the input pathname and the
;;; logical host FROM translations, and then translate the resulting
;;; pathname using the host to the TO translation; this treatment is
;;; recursively invoked until we get a physical pathname, whereupon
;;; our physical DIRECTORY implementation takes over.

;;; FIXME: this is an incomplete implementation.  It only works when
;;; both are logical pathnames (which is OK, because that's the only
;;; case when we call it), but there are other pitfalls as well: see
;;; the DIRECTORY-HELPER below for some, but others include a lack of
;;; pattern handling.
(defun pathname-intersections (one two)
  (aver (logical-pathname-p one))
  (aver (logical-pathname-p two))
  (labels
      ((intersect-version (one two)
         (aver (typep one '(or null (member :newest :wild :unspecific)
                            integer)))
         (aver (typep two '(or null (member :newest :wild :unspecific)
                            integer)))
         (cond
           ((eq one :wild) two)
           ((eq two :wild) one)
           ((or (null one) (eq one :unspecific)) two)
           ((or (null two) (eq two :unspecific)) one)
           ((eql one two) one)
           (t nil)))
       (intersect-name/type (one two)
         (aver (typep one '(or null (member :wild :unspecific) string)))
         (aver (typep two '(or null (member :wild :unspecific) string)))
         (cond
           ((eq one :wild) two)
           ((eq two :wild) one)
           ((or (null one) (eq one :unspecific)) two)
           ((or (null two) (eq two :unspecific)) one)
           ((string= one two) one)
           (t nil)))
       (intersect-directory (one two)
         (aver (typep one '(or null (member :wild :unspecific) list)))
         (aver (typep two '(or null (member :wild :unspecific) list)))
         (cond
           ((eq one :wild) two)
           ((eq two :wild) one)
           ((or (null one) (eq one :unspecific)) two)
           ((or (null two) (eq two :unspecific)) one)
           (t (aver (eq (car one) (car two)))
              (mapcar
               (lambda (x) (cons (car one) x))
               (intersect-directory-helper (cdr one) (cdr two)))))))
    (let ((version (intersect-version
                    (pathname-version one) (pathname-version two)))
          (name (intersect-name/type
                 (pathname-name one) (pathname-name two)))
          (type (intersect-name/type
                 (pathname-type one) (pathname-type two)))
          (host (pathname-host one)))
      (mapcar (lambda (d)
                (make-pathname :host host :name name :type type
                               :version version :directory d))
              (intersect-directory
               (pathname-directory one) (pathname-directory two))))))

;;; FIXME: written as its own function because I (CSR) don't
;;; understand it, so helping both debuggability and modularity.  In
;;; case anyone is motivated to rewrite it, it returns a list of
;;; sublists representing the intersection of the two input directory
;;; paths (excluding the initial :ABSOLUTE or :RELATIVE).
;;;
;;; FIXME: Does not work with :UP or :BACK
;;; FIXME: Does not work with patterns
;;;
;;; FIXME: PFD suggests replacing this implementation with a DFA
;;; conversion of a NDFA.  Find out (a) what this means and (b) if it
;;; turns out to be worth it.
(defun intersect-directory-helper (one two)
  (flet ((simple-intersection (cone ctwo)
           (cond
             ((eq cone :wild) ctwo)
             ((eq ctwo :wild) cone)
             (t (aver (typep cone 'string))
                (aver (typep ctwo 'string))
                (if (string= cone ctwo) cone nil)))))
    (macrolet
        ((loop-possible-wild-inferiors-matches
             (lower-bound bounding-sequence order)
           (let ((index (gensym)) (g2 (gensym)) (g3 (gensym)) (l (gensym)))
             `(let ((,l (length ,bounding-sequence)))
               (loop for ,index from ,lower-bound to ,l
                append (mapcar (lambda (,g2)
                                 (append
                                  (butlast ,bounding-sequence (- ,l ,index))
                                  ,g2))
                        (mapcar
                         (lambda (,g3)
                           (append
                            (if (eq (car (nthcdr ,index ,bounding-sequence))
                                    :wild-inferiors)
                                '(:wild-inferiors)
                                nil) ,g3))
                         (intersect-directory-helper
                          ,@(if order
                                `((nthcdr ,index one) (cdr two))
                                `((cdr one) (nthcdr ,index two)))))))))))
      (cond
        ((and (eq (car one) :wild-inferiors)
              (eq (car two) :wild-inferiors))
         (delete-duplicates
          (append (mapcar (lambda (x) (cons :wild-inferiors x))
                          (intersect-directory-helper (cdr one) (cdr two)))
                  (loop-possible-wild-inferiors-matches 2 one t)
                  (loop-possible-wild-inferiors-matches 2 two nil))
          :test 'equal))
        ((eq (car one) :wild-inferiors)
         (delete-duplicates (loop-possible-wild-inferiors-matches 0 two nil)
                            :test 'equal))
        ((eq (car two) :wild-inferiors)
         (delete-duplicates (loop-possible-wild-inferiors-matches 0 one t)
                            :test 'equal))
        ((and (null one) (null two)) (list nil))
        ((null one) nil)
        ((null two) nil)
        (t (and (simple-intersection (car one) (car two))
                (mapcar (lambda (x) (cons (simple-intersection
                                           (car one) (car two)) x))
                        (intersect-directory-helper (cdr one) (cdr two)))))))))

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
        ;; FIXME: Possibly this MERGE-PATHNAMES call should only
        ;; happen once we get a physical pathname.
        (merged-pathname (merge-pathnames pathname)))
    (labels ((do-physical-directory (pathname)
               (aver (not (logical-pathname-p pathname)))
               (!enumerate-matches (match pathname)
                 (let* ((*ignore-wildcards* t)
                        ;; FIXME: Why not TRUENAME?  As reported by
                        ;; Milan Zamazal sbcl-devel 2003-10-05, using
                        ;; TRUENAME causes a race condition whereby
                        ;; removal of a file during the directory
                        ;; operation causes an error.  It's not clear
                        ;; what the right thing to do is, though.  --
                        ;; CSR, 2003-10-13
                        (truename (probe-file match)))
                   (when truename
                     (setf (gethash (namestring truename) truenames)
                           truename)))))
             (do-directory (pathname)
               (if (logical-pathname-p pathname)
                   (let ((host (intern-logical-host (pathname-host pathname))))
                     (dolist (x (logical-host-canon-transls host))
                       (destructuring-bind (from to) x
                         (let ((intersections
                                (pathname-intersections pathname from)))
                           (dolist (p intersections)
                             (do-directory (translate-pathname p from to)))))))
                   (do-physical-directory pathname))))
      (do-directory merged-pathname))
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
  (let ((pathname (physicalize-pathname (merge-pathnames (pathname pathspec))))
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
                   (let ((namestring (coerce (namestring newpath) 'string)))
                     (when verbose
                       (format *standard-output*
                               "~&creating directory: ~A~%"
                               namestring))
                     (sb!unix:unix-mkdir namestring mode)
                     (unless (probe-file namestring)
                       (restart-case (error 'simple-file-error
                                            :pathname pathspec
                                            :format-control "can't create directory ~A"
                                            :format-arguments (list namestring))
                         (retry ()
                           :report "Retry directory creation."
                           (ensure-directories-exist pathspec :verbose verbose :mode mode))
                         (continue ()
                           :report "Continue as if directory creation was successful."
                           nil)))
                     (setf created-p t)))))
      (values pathspec created-p))))

(/show0 "filesys.lisp 1000")
