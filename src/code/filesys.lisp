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

(in-package "SB-IMPL")

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
;;; Any of these special characters can be preceded by an escape
;;; character to cause it to be treated as a regular character.
(defun remove-escape-characters (namestr start end escape-char)
  "Remove any occurrences of escape characters from the string
   because we've already checked for whatever they may have
   protected."
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
               (cond ((char= char escape-char)
                      (setq quoted t))
                     (t
                      (setf (schar result dst) char)
                      (incf dst)))))))
    (when quoted
      (error 'namestring-parse-error
             :complaint "escape char in a bad place"
             :namestring namestr
             :offset (1- end)))
    (%shrink-vector result dst)))

(defun maybe-make-pattern (namestr start end escape-char)
  (declare (type simple-string namestr)
           (type index start end)
           (type character escape-char))
  (collect ((pattern))
    (let ((quoted nil)
          (any-quotes nil)
          (last-regular-char nil)
          (index start))
      (labels ((regulars ()
                 (if any-quotes
                     (remove-escape-characters namestr last-regular-char
                                               index escape-char)
                     (subseq namestr last-regular-char index)))
               (flush-pending-regulars ()
                 (when last-regular-char
                   (pattern (regulars))
                   (setf any-quotes nil)
                   (setf last-regular-char nil))))
        (loop
         (when (>= index end)
           (return))
         (let ((char (schar namestr index)))
           (cond (quoted
                  (incf index)
                  (setf quoted nil))
                 ((char= char escape-char)
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
        (cond ((null (pattern))
               (if last-regular-char
                   (regulars)
                   ""))
              ((progn
                 (flush-pending-regulars)
                 (null (cdr (pattern))))
               (let ((piece (first (pattern))))
                 (if (eq piece :multi-char-wild)
                     :wild
                     (make-pattern (pattern)))))
              (t
               (make-pattern (pattern))))))))

(declaim (ftype (sfunction ((or (eql :wild) simple-string pattern) character &key (:escape-dot t))
                           simple-string)
                unparse-physical-piece))
(defun unparse-physical-piece (thing escape-char &key escape-dot)
  (let ((length 0)
        (complicated nil))
    (declare (type index length))
    (labels ((needs-escaping-p (char index)
               (or (char= char #\*) (char= char #\?)
                   (char= char #\[) (char= char escape-char)
                   (case escape-dot
                     (:unless-at-start (and (plusp index) (char= char #\.)))
                     ((t) (char= char #\.)))))
             (inspect-fragment (fragment)
               (etypecase fragment
                 ((eql :wild)
                  (incf length)
                  t)
                 (simple-string
                  (incf length (length fragment))
                  (Loop with complicated = nil
                        for char across (the simple-string fragment)
                        for i from 0
                        when (needs-escaping-p char i)
                        do (setf complicated t)
                           (incf length)
                        finally (return complicated)))
                 (pattern
                  (mapcar (lambda (piece)
                            (etypecase piece
                              (simple-string
                               (inspect-fragment piece))
                              ((member :multi-char-wild :single-char-wild)
                               (incf length 1)
                               t)
                              ((cons (eql :character-set))
                               (incf length (+ 2 (length (cdr piece))))
                               t)))
                          (pattern-pieces fragment))))))
      (setf complicated (inspect-fragment thing))
      (unless complicated
        (return-from unparse-physical-piece thing))
      (let ((result (make-string length))
            (index 0))
        (declare (type (simple-array character 1) result)
                 (type index index))
        (labels ((output-character (character)
                   (setf (aref result index) character)
                   (incf index))
                 (output-string (string)
                   (declare (type (simple-array character 1) string))
                   (setf (subseq result index) string)
                   (incf index (length string)))
                 (unparse-fragment (fragment)
                   (etypecase fragment
                     ((eql :wild)
                      (output-character #\*))
                     (simple-string
                      (loop for char across (the simple-string fragment)
                            for i from 0
                            when (needs-escaping-p char i)
                            do (output-character escape-char)
                            do (output-character char)))
                     (pattern
                      (mapc (lambda (piece piece-complicated)
                              (etypecase piece
                                (simple-string
                                 (if piece-complicated
                                     (unparse-fragment piece)
                                     (output-string piece)))
                                ((eql :multi-char-wild)
                                 (output-character #\*))
                                ((eql :single-char-wild)
                                 (output-character #\?))
                                ((cons (eql :character-set))
                                 (output-character #\[)
                                 (output-string (cdr piece))
                                 (output-character #\]))))
                            (pattern-pieces fragment) complicated)))))
          (declare (inline output-character output-string))
          (unparse-fragment thing))
        result))))

(defun make-matcher (piece)
  (cond ((eq piece :wild)
         (constantly t))
        ((typep piece 'pattern)
         (lambda (other)
           (when (stringp other)
             (pattern-matches piece other))))
        (t
         (lambda (other)
           (equal piece other)))))

(defun extract-name-type-and-version (namestr start end escape-char)
  (declare (type simple-string namestr)
           (type index start end))
  (flet ((escape-p (i)
           (and (>= i start) (char= (aref namestr i) escape-char))))
    (let ((last-dot
            (loop for i from (1- end) downto (1+ start)
                  when (and (char= (aref namestr i) #\.)
                            (or (not (escape-p (1- i)))
                                (escape-p (- i 2))))
                  return i)))
      (if last-dot
          (values (maybe-make-pattern namestr start last-dot escape-char)
                  (maybe-make-pattern namestr (1+ last-dot) end escape-char)
                  :newest)
          (values (maybe-make-pattern namestr start end escape-char)
                  nil
                  :newest)))))


;;;; Grabbing the kind of file when we have a native-namestring.
(defun native-file-kind (namestring &optional resolve-symlinks)
  #+win32 (declare (ignore resolve-symlinks))
  #+win32 (nth-value 1 (sb-win32::native-probe-file-name namestring))
  #-win32
  (multiple-value-bind (existsp errno ino mode)
      ;; Note that when resolve-symlinks is true, we'll return NIL if
      ;; there are circular and dangling symlinks anywhere in the
      ;; path. That's different than what our TRUENAME does; this is
      ;; intended as an efficient internal routine.
      (if resolve-symlinks
          (sb-unix:unix-stat namestring)
          (sb-unix:unix-lstat namestring))
    (declare (ignore errno ino))
    (when existsp
      (let ((ifmt (logand mode sb-unix:s-ifmt)))
       (case ifmt
         (#.sb-unix:s-ifreg :file)
         (#.sb-unix:s-ifdir :directory)
         (#.sb-unix:s-iflnk :symlink)
         (t :special))))))

;;;; TRUENAME, PROBE-FILE, FILE-AUTHOR, FILE-WRITE-DATE.

;;; Rewritten in 12/2007 by RMK, replacing 13+ year old CMU code that
;;; made a mess of things in order to support search lists (which SBCL
;;; has never had).  These are now all relatively straightforward
;;; wrappers around stat(2) and realpath(2), with the same basic logic
;;; in all cases.  The wrinkles to be aware of:
;;;
;;; * SBCL defines the truename of an existing, dangling or
;;;   self-referring symlink to be the symlink itself.
;;; * The old version of PROBE-FILE merged the pathspec against
;;;   *DEFAULT-PATHNAME-DEFAULTS* twice, and so lost when *D-P-D*
;;;   was a relative pathname.  Even if the case where *D-P-D* is a
;;;   relative pathname is problematic, there's no particular reason
;;;   to get that wrong, so let's try not to.
;;; * Note that while stat(2) is probably atomic, getting the truename
;;;   for a filename involves poking all over the place, and so is
;;;   subject to race conditions if other programs mutate the file
;;;   system while we're resolving symlinks.  So it's not implausible for
;;;   realpath(3) to fail even if stat(2) succeeded.  There's nothing
;;;   obvious we can do about this, however.
;;; * Windows' apparent analogue of realpath(3) is called
;;;   GetFullPathName, and it's a bit less useful than realpath(3).
;;;   In particular, while realpath(3) errors in case the file doesn't
;;;   exist, GetFullPathName seems to return a filename in all cases.
;;;   As realpath(3) is not atomic anyway, we only ever call it when
;;;   we think a file exists, so just be careful when rewriting this
;;;   routine.
;;;
;;; Given a pathname designator, some quality to query for, return one
;;; of a pathname, a universal time, or a string (a file-author), or
;;; NIL.  QUERY-FOR may be one of :TRUENAME, :EXISTENCE, :WRITE-DATE,
;;; :AUTHOR.  If ERRORP is false, return NIL in case the file system
;;; returns an error code; otherwise, signal an error.  Accepts
;;; logical pathnames, too (but never returns LPNs).  For internal
;;; use.
(defun query-file-system (pathspec query-for &optional (errorp t))
  (block nil
    (tagbody
     retry
       (restart-case
           (let ((pathname (translate-logical-pathname
                            (merge-pathnames
                             (pathname pathspec)
                             (sane-default-pathname-defaults)))))
             (when (wild-pathname-p pathname)
               (sb-kernel::%file-error
                pathname "Can't find the ~A of wild pathname ~A (physicalized from ~A)."
                query-for pathname pathspec))
             (return (%query-file-system pathname query-for errorp)))
         (use-value (value)
           :report "Specify a different path."
           :interactive read-evaluated-form
           (setf pathspec value)
           (go retry))))))

#+win32
(defun %query-file-system (pathname query-for errorp)
  (let ((filename (native-namestring pathname :as-file t)))
    (case query-for
      ((:existence :truename)
       (multiple-value-bind (file kind)
           (sb-win32::native-probe-file-name filename)
         (when (and (not file) kind)
           (setf file filename))
         ;; The following OR was an AND, but that breaks files like NUL,
         ;; for which GetLongPathName succeeds yet GetFileAttributesEx
         ;; fails to return the file kind. --DFL
         (cond
           ((or file kind)
            (values (parse-native-namestring
                     file
                     (pathname-host pathname)
                     (sane-default-pathname-defaults)
                     :as-directory (eq :directory kind))))
           (errorp
            (file-perror filename (sb-win32:get-last-error)
                         "Failed to find the ~A of ~S" query-for filename)))))
      (:write-date
       (cond
         ((sb-win32::native-file-write-date filename))
         (errorp
          (file-perror filename (sb-win32:get-last-error)
                       "Failed to find the ~A of ~S" query-for filename)))))))

#-win32
(defun %query-file-system (pathname query-for errorp)
  (labels ((parse (filename &key as-directory)
             (values (parse-native-namestring
                      filename
                      (pathname-host pathname)
                      (sane-default-pathname-defaults)
                      :as-directory as-directory)))
           (directory-part-realpath (filename)
             ;; So here's a trick: since lstat succeeded, FILENAME
             ;; exists, so its directory exists and only the
             ;; non-directory part is loopy.  So let's resolve
             ;; FILENAME's directory part with realpath(3), in order
             ;; to get a canonical absolute name for the directory,
             ;; and then return a pathname having PATHNAME's name,
             ;; type, and version, but the rest from the truename of
             ;; the directory.  Since we turned PATHNAME into FILENAME
             ;; "as a file", FILENAME does not end in a slash, and so
             ;; we get the directory part of FILENAME by reparsing
             ;; FILENAME and masking off its name, type, and version
             ;; bits.  But note not to call ourselves recursively,
             ;; because we don't want to re-merge against
             ;; *DEFAULT-PATHNAME-DEFAULTS*, since PATHNAME may be a
             ;; relative pathname.
             (multiple-value-bind (realpath errno)
                 (sb-unix:unix-realpath
                  (native-namestring
                   (make-pathname
                    :name :unspecific
                    :type :unspecific
                    :version :unspecific
                    :defaults (parse filename))))
               (cond
                 (realpath
                  (parse realpath :as-directory t))
                 (errorp
                  (file-perror filename errno "Couldn't resolve ~S" filename)))))
           (resolve-problematic-symlink (filename errno realpath-failed)
             ;; SBCL has for many years had a policy that a pathname
             ;; that names an existing, dangling or self-referential
             ;; symlink denotes the symlink itself.  stat(2) fails
             ;; and sets errno to ENOENT or ELOOP respectively, but
             ;; we must distinguish cases where the symlink exists
             ;; from ones where there's a loop in the apparent
             ;; containing directory.
             ;; Also handles symlinks in /proc/pid/fd/ to
             ;; pipes or sockets on Linux
             (multiple-value-bind (linkp ignore ino mode nlink uid gid rdev
                                         size atime mtime)
                 (sb-unix:unix-lstat filename)
               (declare (ignore ignore ino mode nlink gid rdev size atime))
               (cond
                 ((and (or (= errno sb-unix:enoent)
                           (= errno sb-unix:eloop)
                           realpath-failed)
                       linkp)
                  (case query-for
                    (:existence
                     ;; We do this reparse so as to return a
                     ;; normalized pathname.
                     (parse filename))
                    (:truename
                     (let ((realpath (directory-part-realpath filename)))
                       (when realpath
                         (merge-pathnames
                          realpath
                          (if (directory-pathname-p pathname)
                              (parse (car (last (pathname-directory pathname))))
                              pathname)))))
                    (:author (sb-unix:uid-username uid))
                    (:write-date (+ unix-to-universal-time mtime))))
                 ;; The file doesn't exist; maybe error.
                 (errorp
                  (file-perror
                   pathname errno
                   "Failed to find the ~A of ~A" query-for pathname))))))
    (binding* ((filename (native-namestring pathname :as-file t))
               ((existsp errno nil mode nil uid nil nil nil nil mtime)
                (sb-unix:unix-stat filename)))
      (if existsp
          (case query-for
            (:existence
             (parse filename :as-directory (eql (logand mode sb-unix:s-ifmt)
                                                sb-unix:s-ifdir)))
            (:truename
             ;; Note: in case the file is stat'able, POSIX
             ;; realpath(3) gets us a canonical absolute filename,
             ;; even if the post-merge PATHNAME is not absolute
             (parse (or (sb-unix:unix-realpath filename)
                        (resolve-problematic-symlink filename errno t))
                    :as-directory (eql (logand mode sb-unix:s-ifmt)
                                       sb-unix:s-ifdir)))
            (:author (sb-unix:uid-username uid))
            (:write-date (+ unix-to-universal-time mtime)))
          (resolve-problematic-symlink filename errno nil)))))

(defun probe-file (pathspec)
  "Return the truename of PATHSPEC if the truename can be found,
or NIL otherwise.  See TRUENAME for more information."
  (query-file-system pathspec :truename nil))

(defun truename (pathspec)
  "If PATHSPEC is a pathname that names an existing file, return
a pathname that denotes a canonicalized name for the file.  If
pathspec is a stream associated with a file, return a pathname
that denotes a canonicalized name for the file associated with
the stream.

An error of type FILE-ERROR is signalled if no such file exists
or if the file system is such that a canonicalized file name
cannot be determined or if the pathname is wild.

Under Unix, the TRUENAME of a symlink that links to itself or to
a file that doesn't exist is considered to be the name of the
broken symlink itself."
  ;; Note that eventually this routine might be different for streams
  ;; than for other pathname designators.
  (if (streamp pathspec)
      (query-file-system pathspec :truename)
      (query-file-system pathspec :truename)))

(defun file-author (pathspec)
  "Return the author of the file specified by PATHSPEC. Signal an
error of type FILE-ERROR if no such file exists, or if PATHSPEC
is a wild pathname."
  (query-file-system pathspec :author))

(defun file-write-date (pathspec)
  "Return the write date of the file specified by PATHSPEC.
An error of type FILE-ERROR is signaled if no such file exists,
or if PATHSPEC is a wild pathname."
  (query-file-system pathspec :write-date))

;;;; miscellaneous other operations

(defun rename-file (file new-name)
  "Rename FILE to have the specified NEW-NAME. If FILE is a stream open to a
file, then the associated file is renamed."
  (let* ((original (merge-pathnames file (sane-default-pathname-defaults)))
         (old-truename (truename original))
         (original-namestring (native-namestring (physicalize-pathname original)
                                                 :as-file t))
         (new-name (merge-pathnames new-name original))
         (new-namestring (native-namestring (physicalize-pathname new-name)
                                            :as-file t)))
    (unless new-namestring
      (sb-kernel::%file-error new-name "~S can't be created." new-name))
    (multiple-value-bind (res error)
        (sb-unix:unix-rename original-namestring new-namestring)
      (unless res
        (file-perror
         new-name error
         "~@<couldn't rename ~2I~_~A ~I~_to ~2I~_~A~:>" original new-name))
      (when (streamp file)
        (file-name file new-name))
      (values new-name old-truename (truename new-name)))))

(defun delete-file (file)
  "Delete the specified FILE.

If FILE is a stream, on Windows the stream is closed immediately. On Unix
platforms the stream remains open, allowing IO to continue: the OS resources
associated with the deleted file remain available till the stream is closed as
per standard Unix unlink() behaviour."
  (let* ((pathname (translate-logical-pathname
                    (merge-pathnames file (sane-default-pathname-defaults))))
         (namestring (native-namestring pathname :as-file t)))
    #+win32
    (when (streamp file)
      (close file))
    (multiple-value-bind (res err)
        #-win32 (sb-unix:unix-unlink namestring)
        #+win32 (or (sb-win32::native-delete-file namestring)
                     (values nil (sb-win32:get-last-error)))
        (unless res
          (with-simple-restart (continue "Return T")
            (file-perror namestring err 'delete-file-error)))))
  t)

(defun directorize-pathname (pathname)
  (cond
    ((wild-pathname-p pathname)
     (sb-kernel::%file-error
      pathname "Cannot compute directory pathname for wild pathname ~S" pathname))
    ((let* ((name (pathname-name pathname))
            (namep (pathname-component-present-p name))
            (type (pathname-type pathname))
            (typep (pathname-component-present-p type)))
       (when (or namep typep)
         (let ((from-file (format nil "~:[~*~;~A~]~:[~*~;.~A~]"
                                  namep name typep type)))
           (make-pathname
            :host (pathname-host pathname)
            :device (pathname-device pathname)
            :directory (append (pathname-directory pathname)
                               (list from-file)))))))
    (t
     pathname)))

(defun delete-directory (pathspec &key recursive)
  "Deletes the directory designated by PATHSPEC (a pathname designator).
Returns the truename of the directory deleted.

If RECURSIVE is false \(the default), signals an error unless the directory is
empty. If RECURSIVE is true, first deletes all files and subdirectories. If
RECURSIVE is true and the directory contains symbolic links, the links are
deleted, not the files and directories they point to.

Signals an error if PATHSPEC designates a file or a symbolic link instead of a
directory, or if the directory could not be deleted for any reason.

Both

   \(DELETE-DIRECTORY \"/tmp/foo\")
   \(DELETE-DIRECTORY \"/tmp/foo/\")

delete the \"foo\" subdirectory of \"/tmp\", or signal an error if it does not
exist or if is a file or a symbolic link."
  (declare (type pathname-designator pathspec))
  (labels ((recurse-merged (dir)
             (lambda (sub)
               (recurse (merge-pathnames sub dir))))
           (delete-merged (dir)
             (lambda (file)
               (delete-file (merge-pathnames file dir))))
           (recurse (dir)
             (map-directory (recurse-merged dir) dir
                            :files nil
                            :directories t
                            :classify-symlinks nil)
             (map-directory (delete-merged dir) dir
                            :files t
                            :directories nil
                            :classify-symlinks nil)
             (delete-dir dir))
           (delete-dir (dir)
             (let ((namestring (native-namestring dir :as-file t)))
               (multiple-value-bind (res errno)
                 #+win32
                 (or (sb-win32::native-delete-directory namestring)
                     (values nil (sb-win32:get-last-error)))
                 #-win32
                 (values
                  (not (minusp (alien-funcall
                                (extern-alien "rmdir"
                                              (function int c-string))
                                namestring)))
                  (get-errno))
                 (if res
                     dir
                     (file-perror namestring errno
                                  "Could not delete directory ~S" namestring))))))
    (let ((physical (directorize-pathname
                     (physicalize-pathname
                      (merge-pathnames
                       pathspec (sane-default-pathname-defaults))))))
      (if recursive
          (recurse physical)
          (delete-dir physical)))))


(defun sbcl-homedir-pathname ()
  sb-sys::*sbcl-homedir-pathname*)

(defun %sbcl-homedir-pathname ()
  ;; Should we absoluteize this if it was obtained automatically?
  ;; Depends whether people are in the habit of using chdir within Lisp.
  (labels ((parse (namestring &optional (directory t))
             (parse-native-namestring namestring
                                      *physical-host*
                                      *default-pathname-defaults*
                                      :as-directory directory))
           (probe (path)
             (let ((contrib (merge-pathnames "contrib/" path)))
               (when (probe-file contrib)
                 path)))
           (try-runtime-home (path)
             (or (probe path)
                 (probe (merge-pathnames "../lib/sbcl/" path)))))
    (let* ((env (posix-getenv "SBCL_HOME"))
           (env (and env (not (string= env ""))
                     (parse env))))
      (or (and env
               (probe env))
          (try-runtime-home (parse (or (extern-alien "sbcl_runtime_home" c-string)
                                       "")))
          (probe (parse "."))
          ;; Handle a symlink
          (let ((runtime (extern-alien "sbcl_runtime" c-string)))
            (and runtime
                 (let ((truename (probe-file (parse runtime nil))))
                   (when truename
                    (try-runtime-home (make-pathname :name nil
                                                     :type nil
                                                     :defaults truename))))))))))

(flet ((not-empty (x)
         (and (not (equal x "")) x))
       (lose (&optional username)
         (error "Couldn't find home directory~@[ for ~S~]." username)))

  #-win32
  (defun user-homedir-namestring (&optional username)
    (if username
        (sb-unix:user-homedir username)
        (or (not-empty (posix-getenv "HOME"))
            (not-empty (sb-unix:uid-homedir (sb-unix:unix-getuid)))
            (lose))))

  #+win32
  (defun user-homedir-namestring (&optional username)
    (if username
        (lose username)
        (or (not-empty (posix-getenv "HOME"))
            (not-empty (posix-getenv "USERPROFILE"))
            (let ((drive (not-empty (posix-getenv "HOMEDRIVE")))
                  (path (not-empty (posix-getenv "HOMEPATH"))))
              (and drive path
                   (concatenate 'string drive path)))
            (lose)))))

;;; (This is an ANSI Common Lisp function.)
(defun user-homedir-pathname (&optional host)
  "Return the home directory of the user as a pathname. If the HOME
environment variable has been specified, the directory it designates
is returned; otherwise obtains the home directory from the operating
system. HOST argument is ignored by SBCL."
  (declare (ignore host))
  (values
   (parse-native-namestring
    (or (user-homedir-namestring)
        #+win32
        (sb-win32::get-folder-namestring sb-win32::csidl_profile))
    *physical-host*
    *default-pathname-defaults*
    :as-directory t)))


;;;; DIRECTORY

(defun directory (pathspec &key (resolve-symlinks t))
  "Return a list of PATHNAMEs, each the TRUENAME of a file matching PATHSPEC.

Note that the interaction between this ANSI-specified TRUENAMEing and
the semantics of the Unix filesystem (symbolic links..) means this
function can sometimes return files which don't have the same
directory as PATHSPEC.

If :RESOLVE-SYMLINKS is NIL, don't resolve symbolic links in matching
filenames."
  (let (;; We create one entry in this hash table for each truename,
        ;; as an asymptotically efficient way of removing duplicates
        ;; (which can arise when e.g. multiple symlinks map to the
        ;; same truename).
        (truenames (make-hash-table :test #'equal)))
    (labels ((record (pathname)
               (let ((truename (if resolve-symlinks
                                   ;; FIXME: Why not not TRUENAME?  As reported by
                                   ;; Milan Zamazal sbcl-devel 2003-10-05, using
                                   ;; TRUENAME causes a race condition whereby
                                   ;; removal of a file during the directory
                                   ;; operation causes an error.  It's not clear
                                   ;; what the right thing to do is, though.  --
                                   ;; CSR, 2003-10-13
                                   (%query-file-system pathname :truename nil)
                                   (%query-file-system pathname :existence nil))))
                 (when truename
                   (setf (gethash (namestring truename) truenames)
                         truename))))
             (do-physical-pathnames (pathname)
               (aver (not (logical-pathname-p pathname)))
               (let* (;; KLUDGE: Since we don't canonize pathnames on construction,
                      ;; we really have to do it here to get #p"foo/." mean the same
                      ;; as #p"foo/./".
                      (pathname (canonicalize-pathname pathname))
                      (name (pathname-name pathname))
                      (type (pathname-type pathname))
                      (match-name (make-matcher name))
                      (match-type (make-matcher type)))
                 (map-matching-directories
                  (if (or name type)
                      (lambda (directory)
                        (map-matching-entries #'record
                                              directory
                                              match-name
                                              match-type))
                      #'record)
                  pathname)))
             (do-pathnames (pathname)
               (if (logical-pathname-p pathname)
                   (let ((host (intern-logical-host (pathname-host pathname))))
                     (dolist (x (logical-host-canon-transls host))
                       (destructuring-bind (from to) x
                         (let ((intersections
                                (pathname-intersections pathname from)))
                           (dolist (p intersections)
                             (do-pathnames (translate-pathname p from to)))))))
                   (do-physical-pathnames pathname))))
      (declare (truly-dynamic-extent #'record))
      (do-pathnames (merge-pathnames pathspec)))
    ;; Sorting isn't required by the ANSI spec, but sorting into some
    ;; canonical order seems good just on the grounds that the
    ;; implementation should have repeatable behavior when possible.
    (let ((result (sort (loop for namestring being each hash-key in truenames
                              using (hash-value truename)
                              collect (cons namestring truename))
                        #'string< :key #'car)))
      (map-into result #'cdr result))))

(defun canonicalize-pathname (pathname)
  ;; We're really only interested in :UNSPECIFIC -> NIL, :BACK and :UP,
  ;; and dealing with #p"foo/.." and #p"foo/."
  (labels ((simplify (piece)
             (unless (eq :unspecific piece)
               piece))
           (canonicalize-directory (directory)
             (let (pieces)
               (dolist (piece directory)
                 (cond
                    ((and pieces (member piece '(:back :up)))
                     ;; FIXME: We should really canonicalize when we construct
                     ;; pathnames. This is just wrong.
                     (case (car pieces)
                       ((:absolute :wild-inferiors)
                        (sb-kernel::%file-error
                         pathname
                         "Invalid use of ~S after ~S." piece (car pieces)))
                       ((:relative :up :back)
                        (push piece pieces))
                       (t
                        (pop pieces))))
                    ((equal piece ".")
                     ;; This case only really matters on Windows,
                     ;; because on POSIX, our call site (TRUENAME via
                     ;; QUERY-FILE-SYSTEM) only passes in pathnames from
                     ;; realpath(3), in which /./ has been removed
                     ;; already.  Windows, however, depends on us to
                     ;; perform this fixup. -- DFL
                     )
                    (t
                     (push piece pieces))))
               (nreverse pieces))))
    (let ((name (simplify (pathname-name pathname)))
          (type (simplify (pathname-type pathname)))
          (dir (canonicalize-directory (pathname-directory pathname))))
      (cond
        ((not (equal "." name))
         (make-pathname :name name :type type :directory dir
                        :defaults pathname))
        ((not type)
         (make-pathname :name nil :defaults pathname))
        ((equal "" type)
         (make-pathname :name nil :type nil :directory (butlast dir)
                        :defaults pathname))
        (t
         (make-pathname :name name :type type :directory dir
                        :defaults pathname))))))

;;; Given a native namestring, provides a WITH-HASH-TABLE-ITERATOR style
;;; interface to mapping over namestrings of entries in the corresponding
;;; directory.
(defmacro with-native-directory-iterator ((iterator namestring &key errorp) &body body)
  (with-unique-names (one-iter)
    `(dx-flet
         ((iterate (,one-iter)
            (declare (type function ,one-iter))
            (macrolet ((,iterator ()
                         `(funcall ,',one-iter)))
              ,@body)))
       #+win32
       (sb-win32::native-call-with-directory-iterator
        #'iterate ,namestring ,errorp)
       #-win32
       (call-with-native-directory-iterator #'iterate ,namestring ,errorp))))

(defun call-with-native-directory-iterator (function namestring errorp)
  (declare (type (or null string) namestring)
           (function function))
  (let (dp)
    (when namestring
      (dx-flet
          ((one-iter ()
             (tagbody
              :next
                (let ((ent (sb-unix:unix-readdir dp nil)))
                  (when ent
                    (let ((name (sb-unix:unix-dirent-name ent)))
                      (when name
                        (cond ((equal "." name)
                               (go :next))
                              ((equal ".." name)
                               (go :next))
                              (t
                               (return-from one-iter name))))))))))
        (unwind-protect
             (progn
               (setf dp (sb-unix:unix-opendir namestring errorp))
               (when dp
                 (funcall function #'one-iter)))
          (when dp
            (sb-unix:unix-closedir dp nil)))))))

;;; This is our core directory access interface that we use to implement
;;; DIRECTORY.
(defun map-directory (function directory &key (files t) (directories t)
                      (classify-symlinks t) (errorp t))
  "Map over entries in DIRECTORY. Keyword arguments specify which entries to
map over, and how:

 :FILES
    If true, call FUNCTION with the pathname of each file in DIRECTORY.
    Defaults to T.

 :DIRECTORIES
   If true, call FUNCTION with a pathname for each subdirectory of DIRECTORY.
   If :AS-FILES, the pathname used is a pathname designating the subdirectory
   as a file in DIRECTORY. Otherwise the pathname used is a directory
   pathname. Defaults to T.

 :CLASSIFY-SYMLINKS
   If true, the decision to call FUNCTION with the pathname of a symbolic link
   depends on the resolution of the link: if it points to a directory, it is
   considered a directory entry, otherwise a file entry. If false, all
   symbolic links are considered file entries. In both cases the pathname used
   for the symbolic link is not fully resolved, but names it as an immediate
   child of DIRECTORY. Defaults to T.

 :ERRORP
   If true, signal an error if DIRECTORY does not exist, cannot be read, etc.
   Defaults to T.

Experimental: interface subject to change."
  (declare (pathname-designator directory))
  (let* ((fun (%coerce-callable-to-fun function))
         (as-files (eq :as-files directories))
         (physical (physicalize-pathname directory))
         (realname (%query-file-system physical :existence nil))
         (canonical (if realname
                        (parse-native-namestring realname
                                                 (pathname-host physical)
                                                 (sane-default-pathname-defaults)
                                                 :as-directory t)
                        (return-from map-directory nil)))
         (dirname (native-namestring canonical)))
    (flet ((map-it (name dirp)
             (funcall fun
                      (merge-pathnames (parse-native-namestring
                                        name nil physical
                                        :as-directory (and dirp (not as-files)))
                                       physical))))
      (with-native-directory-iterator (next dirname :errorp errorp)
        (loop
          ;; provision for FindFirstFileExW-based iterator that should be used
          ;; on Windows: file kind is known instantly there, so we'll have it
          ;; returned by (next) soon.
          (multiple-value-bind (name kind) (next)
            (unless (or name kind) (return))
            (unless kind
              (setf kind (native-file-kind
                          (concatenate 'string dirname name))))
            (when kind
              (case kind
                (:directory
                 (when directories
                   (map-it name t)))
                (:symlink
                 (if classify-symlinks
                     (let* ((tmpname (merge-pathnames
                                      (parse-native-namestring
                                       name nil physical :as-directory nil)
                                      physical))
                            (truename (%query-file-system tmpname :truename nil)))
                       (if (or (not truename)
                               (or (pathname-name truename) (pathname-type truename)))
                           (when files
                             (funcall fun tmpname))
                           (when directories
                             (map-it name t))))
                     (when files
                       (map-it name nil))))
                (t
                 ;; Anything else parses as a file.
                 (when files
                   (map-it name nil)))))))))))

;;; Part of DIRECTORY: implements matching the directory spec. Calls FUNCTION
;;; with all DIRECTORIES that match the directory portion of PATHSPEC.
(defun map-matching-directories (function pathspec)
  (binding* ((directory (pathname-directory pathspec))
             ((mode end) (loop for component in directory
                            for i :of-type index from 0
                            do (typecase component
                                 ((or (eql :wild) pattern)
                                  (return (values :wild i)))
                                 ((eql :wild-inferiors)
                                  (return (values :wild-inferiors i))))))
             ((directory-start directory-rest)
              (if end
                  (values (subseq directory 0 end) (subseq directory end))
                  (values directory directory)))
             (starting-point (make-pathname :directory directory-start
                                            :device (pathname-device pathspec)
                                            :host (pathname-host pathspec)
                                            :name nil
                                            :type nil
                                            :version nil)))
    (case mode
      (:wild-inferiors
       (map-wild-inferiors function directory-rest starting-point))
      (:wild
       (map-wild function directory-rest starting-point))
      (t ; Nothing wild -- the directory matches itself.
       (funcall function starting-point))))
  nil)

(defun last-directory-piece (pathname)
  (car (last (pathname-directory pathname))))

;;; Part of DIRECTORY: implements iterating over a :WILD or pattern component
;;; in the directory spec.
(defun map-wild (function more directory)
  (let ((this (pop more))
        (next (car more)))
    (flet ((cont (subdirectory)
             (cond ((not more)
                    ;; end of the line
                    (funcall function subdirectory))
                   ((or (eq :wild next) (typep next 'pattern))
                    (map-wild function more subdirectory))
                   ((eq :wild-inferiors next)
                    (map-wild-inferiors function more subdirectory))
                   (t
                    (let ((this (pathname-directory subdirectory)))
                      (map-matching-directories
                       function
                       (make-pathname :directory (append this more)
                                      :defaults subdirectory)))))))
      (map-directory
       (if (eq :wild this)
           #'cont
           (lambda (sub)
             (when (pattern-matches this (last-directory-piece sub))
               (cont sub))))
       directory
       :files nil
       :directories t
       :errorp nil))))

;;; Part of DIRECTORY: implements iterating over a :WILD-INFERIORS component
;;; in the directory spec.
(defun map-wild-inferiors (function more directory)
  (loop while (member (car more) '(:wild :wild-inferiors))
        do (pop more))
  (let ((next (car more))
        (rest (cdr more)))
    (unless more
      (funcall function directory))
    (map-directory
     (cond ((not more)
            (lambda (pathname)
              (funcall function pathname)
              (map-wild-inferiors function more pathname)))
           (t
            (lambda (pathname)
              (let ((this (pathname-directory pathname)))
                (when (equal next (car (last this)))
                  (map-matching-directories
                   function
                   (make-pathname :directory (append this rest)
                                  :defaults pathname)))
                (map-wild-inferiors function more pathname)))))
     directory
     :files nil
     :directories t
     :errorp nil)))

;;; Part of DIRECTORY: implements iterating over entries in a directory, and
;;; matching them.
(defun map-matching-entries (function directory match-name match-type)
  (map-directory
   (lambda (file)
     (when (and (funcall match-name (pathname-name file))
                (funcall match-type (pathname-type file)))
       (funcall function file)))
   directory
   :files t
   :directories :as-files
   :errorp nil))

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

;;; The above was written by CSR, I (RMK) believe.  The argument that
;;; motivates the interpretation is faulty, however: PATHNAME-MATCH-P
;;; returns true for (PATHNAME-MATCH-P #P"/tmp/*/" #P"/tmp/../"), but
;;; the latter pathname is not in the result of DIRECTORY on the
;;; former.  Indeed, if DIRECTORY were constrained to return the
;;; truename for every pathname for which PATHNAME-MATCH-P returned
;;; true and which denoted a filename that named an existing file,
;;; (DIRECTORY #P"/tmp/**/") would be required to list every file on a
;;; Unix system, since any file can be named as though it were "below"
;;; /tmp, given the dotdot entries.  So I think the strongest
;;; "consistency" we can define between PATHNAME-MATCH-P and DIRECTORY
;;; is that PATHNAME-MATCH-P returns true of everything DIRECTORY
;;; returns, but not vice versa.

;;; In any case, even if the motivation were sound, DIRECTORY on a
;;; wild logical pathname has no portable semantics.  I see nothing in
;;; ANSI that requires implementations to support wild physical
;;; pathnames, and so there need not be any translation of a wild
;;; logical pathname to a phyiscal pathname.  So a program that calls
;;; DIRECTORY on a wild logical pathname is doing something
;;; non-portable at best.  And if the only sensible semantics for
;;; DIRECTORY on a wild logical pathname is something like the
;;; following, it would be just as well if it signaled an error, since
;;; a program can't possibly rely on the result of an intersection of
;;; user-defined translations with a file system probe.  (Potentially
;;; useful kinds of "pathname" that might not support wildcards could
;;; include pathname hosts that model unqueryable namespaces like HTTP
;;; URIs, or that model namespaces that it's not convenient to
;;; investigate, such as the namespace of TCP ports that some network
;;; host listens on.  I happen to think it a bad idea to try to
;;; shoehorn such namespaces into a pathnames system, but people
;;; sometimes claim to want pathnames for these things.)  -- RMK
;;; 2007-12-31.

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
           ((not (pathname-component-present-p one)) two)
           ((not (pathname-component-present-p two)) one)
           ((eql one two) one)
           (t nil)))
       (intersect-name/type (one two)
         (aver (typep one '(or null (member :wild :unspecific) string)))
         (aver (typep two '(or null (member :wild :unspecific) string)))
         (cond
           ((eq one :wild) two)
           ((eq two :wild) one)
           ((not (pathname-component-present-p one)) two)
           ((not (pathname-component-present-p two)) one)
           ((string= one two) one)
           (t (return-from pathname-intersections nil))))
       (intersect-directory (one two)
         (aver (typep one '(or null (member :wild :unspecific) list)))
         (aver (typep two '(or null (member :wild :unspecific) list)))
         (cond
           ((eq one :wild) two)
           ((eq two :wild) one)
           ((not (pathname-component-present-p one)) two)
           ((not (pathname-component-present-p two)) one)
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


(defun directory-pathname-p (pathname)
  (and (pathnamep pathname)
       (null (pathname-name pathname))
       (null (pathname-type pathname))))

;; FIXME: repeatedly poking at the file system ought to be
;; unnecessary, and walking the directory hierarchy from the top down
;; is probably a waste of effort in most cases. If we can trust mkdir
;; returning ENOENT and EEXIST always and only where it's supposed to,
;; we could get this down to two syscalls in the optimal success case
;; case, one in the pessimal failure case, and no worse than this in a
;; hypothetical average. (It looks like we inherited this approach
;; from CMUCL; maybe some ancient Unix's mkdir errno values weren't
;; reliable?)
(defun ensure-directories-exist (pathspec &key verbose (mode #o777))
  "Test whether the directories containing the specified file
  actually exist, and attempt to create them if they do not.
  The MODE argument is a CMUCL/SBCL-specific extension to control
  the Unix permission bits."
  (let ((pathname (physicalize-pathname (merge-pathnames (pathname pathspec))))
        (created-p nil))
    (when (wild-pathname-p pathname)
      (sb-kernel::%file-error pathspec "bad place for a wild pathname"))
    (let* ((host (pathname-host pathname))
           (dir (pathname-directory pathname))
           (dev (pathname-device pathname)))
      (loop for i from (case dev (:unc 3) (otherwise 2))
              upto (length dir)
            do
            (let* ((newpath (make-pathname
                             :host host
                             :device dev
                             :directory (subseq dir 0 i)))
                   (namestring (coerce (native-namestring newpath :as-file t)
                                       'string))
                   (kind (native-file-kind namestring t)))
              (unless (eq :directory kind)
                (when verbose
                  (format *standard-output*
                          "~&creating directory: ~A~%"
                          namestring))
                (sb-unix:unix-mkdir namestring mode)
                (let ((newkind (native-file-kind namestring t)))
                  (unless (eq :directory newkind)
                    (restart-case
                        (sb-kernel::%file-error
                         pathspec
                         "Can't create directory ~A~:[~;,~%a file with ~
                          the same name already exists.~]"
                         namestring
                         (and kind (not (eq :directory newkind))))
                      (retry ()
                        :report "Retry directory creation."
                        (ensure-directories-exist
                         pathspec :verbose verbose :mode mode)))))
                (setf created-p t))))
      (values pathspec created-p))))
