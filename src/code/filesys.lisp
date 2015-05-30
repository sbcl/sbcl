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
;;; Any of these special characters can be preceded by an escape
;;; character to cause it to be treated as a regular character.
(defun remove-escape-characters (namestr start end escape-char)
  #!+sb-doc
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
      (flet ((flush-pending-regulars ()
               (when last-regular-char
                 (pattern (if any-quotes
                              (remove-escape-characters
                               namestr last-regular-char
                               index escape-char)
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
           (make-pattern (pattern))))))

(defun unparse-physical-piece (thing escape-char)
  (etypecase thing
    ((member :wild) "*")
    (simple-string
     (let* ((srclen (length thing))
            (dstlen srclen))
       (dotimes (i srclen)
         (let ((char (schar thing i)))
           (case char
             ((#\* #\? #\[)
              (incf dstlen))
             (t (when (char= char escape-char)
                  (incf dstlen))))))
       (let ((result (make-string dstlen))
             (dst 0))
         (dotimes (src srclen)
           (let ((char (schar thing src)))
             (case char
               ((#\* #\? #\[)
                (setf (schar result dst) escape-char)
                (incf dst))
               (t (when (char= char escape-char)
                    (setf (schar result dst) escape-char)
                    (incf dst))))
             (setf (schar result dst) char)
             (incf dst)))
         result)))
    (pattern
     (with-simple-output-to-string (s)
       (dolist (piece (pattern-pieces thing))
         (etypecase piece
           (simple-string
            (write-string piece s))
           (symbol
            (ecase piece
              (:multi-char-wild
               (write-string "*" s))
              (:single-char-wild
               (write-string "?" s))))
           (cons
            (case (car piece)
              (:character-set
               (write-string "[" s)
               (write-string (cdr piece) s)
               (write-string "]" s))
              (t
               (error "invalid pattern piece: ~S" piece))))))))))

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

(/show0 "filesys.lisp 160")

(defun extract-name-type-and-version (namestr start end escape-char)
  (declare (type simple-string namestr)
           (type index start end))
  (let* ((last-dot (position #\. namestr :start (1+ start) :end end
                             :from-end t)))
    (cond
      (last-dot
       (values (maybe-make-pattern namestr start last-dot escape-char)
               (maybe-make-pattern namestr (1+ last-dot) end escape-char)
               :newest))
      (t
       (values (maybe-make-pattern namestr start end escape-char)
               nil
               :newest)))))

(/show0 "filesys.lisp 200")


;;;; Grabbing the kind of file when we have a namestring.
(defun native-file-kind (namestring)
  (multiple-value-bind (existsp errno ino mode)
      #!-win32
      (sb!unix:unix-lstat namestring)
      #!+win32
      (sb!unix:unix-stat namestring)
    (declare (ignore errno ino))
    (when existsp
      (let ((ifmt (logand mode sb!unix:s-ifmt)))
       (case ifmt
         (#.sb!unix:s-ifreg :file)
         (#.sb!unix:s-ifdir :directory)
         #!-win32
         (#.sb!unix:s-iflnk :symlink)
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
  (let ((pathname (translate-logical-pathname
                   (merge-pathnames
                    (pathname pathspec)
                    (sane-default-pathname-defaults)))))
    (when (wild-pathname-p pathname)
      (error 'simple-file-error
             :pathname pathname
             :format-control "~@<can't find the ~A of wild pathname ~A~
                              (physicalized from ~A).~:>"
             :format-arguments (list query-for pathname pathspec)))
    (macrolet ((fail (note-format pathname errno)
                 ;; Do this as a macro to avoid evaluating format
                 ;; calls when ERROP is NIL
                 `(if errorp
                      (simple-file-perror ,note-format ,pathname ,errno)
                      (return-from query-file-system nil))))
      (let ((filename (native-namestring pathname :as-file t)))
        #!+win32
        (case query-for
          ((:existence :truename)
           (multiple-value-bind (file kind)
               (sb!win32::native-probe-file-name filename)
             (when (and (not file) kind)
               (setf file filename))
             ;; The following OR was an AND, but that breaks files like NUL,
             ;; for which GetLongPathName succeeds yet GetFileAttributesEx
             ;; fails to return the file kind. --DFL
             (if (or file kind)
                 (values
                  (parse-native-namestring
                   file
                   (pathname-host pathname)
                   (sane-default-pathname-defaults)
                   :as-directory (eq :directory kind)))
                 (fail (format nil "Failed to find the ~A of ~~A" query-for) filename
                       (sb!win32:get-last-error)))))
          (:write-date
           (or (sb!win32::native-file-write-date filename)
               (fail (format nil "Failed to find the ~A of ~~A" query-for) filename
                       (sb!win32:get-last-error)))))
        #!-win32
        (multiple-value-bind (existsp errno ino mode nlink uid gid rdev size
                                      atime mtime)
            (sb!unix:unix-stat filename)
          (declare (ignore ino nlink gid rdev size atime))
          (labels ((parse (filename &key (as-directory
                                          (eql (logand mode
                                                       sb!unix:s-ifmt)
                                               sb!unix:s-ifdir)))
                     (values
                      (parse-native-namestring
                       filename
                       (pathname-host pathname)
                       (sane-default-pathname-defaults)
                       :as-directory as-directory)))
                   (resolve-problematic-symlink (&optional realpath-failed)
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
                         (sb!unix:unix-lstat filename)
                       (declare (ignore ignore ino mode nlink gid rdev size atime))
                       (when (and (or (= errno sb!unix:enoent)
                                      (= errno sb!unix:eloop)
                                      realpath-failed)
                                  linkp)
                         (return-from query-file-system
                           (case query-for
                             (:existence
                              ;; We do this reparse so as to return a
                              ;; normalized pathname.
                              (parse filename :as-directory nil))
                             (:truename
                              ;; So here's a trick: since lstat succeded,
                              ;; FILENAME exists, so its directory exists and
                              ;; only the non-directory part is loopy.  So
                              ;; let's resolve FILENAME's directory part with
                              ;; realpath(3), in order to get a canonical
                              ;; absolute name for the directory, and then
                              ;; return a pathname having PATHNAME's name,
                              ;; type, and version, but the rest from the
                              ;; truename of the directory.  Since we turned
                              ;; PATHNAME into FILENAME "as a file", FILENAME
                              ;; does not end in a slash, and so we get the
                              ;; directory part of FILENAME by reparsing
                              ;; FILENAME and masking off its name, type, and
                              ;; version bits.  But note not to call ourselves
                              ;; recursively, because we don't want to
                              ;; re-merge against *DEFAULT-PATHNAME-DEFAULTS*,
                              ;; since PATHNAME may be a relative pathname.
                              (merge-pathnames
                               (parse
                                (multiple-value-bind (realpath errno)
                                    (sb!unix:unix-realpath
                                     (native-namestring
                                      (make-pathname
                                       :name :unspecific
                                       :type :unspecific
                                       :version :unspecific
                                       :defaults (parse filename
                                                        :as-directory nil))))
                                  (or realpath
                                      (fail "couldn't resolve ~A" filename errno)))
                                :as-directory t)
                               (if (directory-pathname-p pathname)
                                   (parse (car (last (pathname-directory pathname)))
                                          :as-directory nil)
                                   pathname)))
                             (:author (sb!unix:uid-username uid))
                             (:write-date (+ unix-to-universal-time mtime))))))
                     ;; If we're still here, the file doesn't exist; error.
                     (fail
                      (format nil "Failed to find the ~A of ~~A" query-for)
                      pathspec errno)))
            (if existsp
                (case query-for
                  (:existence (parse filename))
                  (:truename
                   ;; Note: in case the file is stat'able, POSIX
                   ;; realpath(3) gets us a canonical absolute
                   ;; filename, even if the post-merge PATHNAME
                   ;; is not absolute
                   (parse (or (sb!unix:unix-realpath filename)
                              (resolve-problematic-symlink t))))
                  (:author (sb!unix:uid-username uid))
                  (:write-date (+ unix-to-universal-time mtime)))
                (resolve-problematic-symlink))))))))


(defun probe-file (pathspec)
  #!+sb-doc
  "Return the truename of PATHSPEC if the truename can be found,
or NIL otherwise.  See TRUENAME for more information."
  (query-file-system pathspec :truename nil))

(defun truename (pathspec)
  #!+sb-doc
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
  #!+sb-doc
  "Return the author of the file specified by PATHSPEC. Signal an
error of type FILE-ERROR if no such file exists, or if PATHSPEC
is a wild pathname."
  (query-file-system pathspec :author))

(defun file-write-date (pathspec)
  #!+sb-doc
  "Return the write date of the file specified by PATHSPEC.
An error of type FILE-ERROR is signaled if no such file exists,
or if PATHSPEC is a wild pathname."
  (query-file-system pathspec :write-date))

;;;; miscellaneous other operations

(/show0 "filesys.lisp 700")

(defun rename-file (file new-name)
  #!+sb-doc
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
      (values new-name old-truename (truename new-name)))))

(defun delete-file (file)
  #!+sb-doc
  "Delete the specified FILE.

If FILE is a stream, on Windows the stream is closed immediately. On Unix
platforms the stream remains open, allowing IO to continue: the OS resources
associated with the deleted file remain available till the stream is closed as
per standard Unix unlink() behaviour."
  (let* ((pathname (translate-logical-pathname
                    (merge-pathnames file (sane-default-pathname-defaults))))
         (namestring (native-namestring pathname :as-file t)))
    #!+win32
    (when (streamp file)
      (close file))
    (multiple-value-bind (res err)
        #!-win32 (sb!unix:unix-unlink namestring)
        #!+win32 (or (sb!win32::native-delete-file namestring)
                     (values nil (sb!win32:get-last-error)))
        (unless res
          (simple-file-perror "couldn't delete ~A" namestring err))))
  t)

(defun directorize-pathname (pathname)
  (if (or (pathname-name pathname)
          (pathname-type pathname))
      (make-pathname :directory (append (pathname-directory pathname)
                                        (list (file-namestring pathname)))
                     :host (pathname-host pathname)
                     :device (pathname-device pathname))
      pathname))

(defun delete-directory (pathspec &key recursive)
  #!+sb-doc
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
  (let ((physical (directorize-pathname
                   (physicalize-pathname
                    (merge-pathnames
                     pathspec (sane-default-pathname-defaults))))))
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
                     #!+win32
                     (or (sb!win32::native-delete-directory namestring)
                         (values nil (sb!win32:get-last-error)))
                     #!-win32
                     (values
                      (not (minusp (alien-funcall
                                    (extern-alien "rmdir"
                                                  (function int c-string))
                                    namestring)))
                      (get-errno))
                     (if res
                         dir
                         (simple-file-perror
                          "Could not delete directory ~A"
                          namestring errno))))))
      (if recursive
          (recurse physical)
          (delete-dir physical)))))


(defun sbcl-homedir-pathname ()
  (let ((sbcl-home (posix-getenv "SBCL_HOME")))
    ;; SBCL_HOME isn't set for :EXECUTABLE T embedded cores
    (when (and sbcl-home (not (string= sbcl-home "")))
      (parse-native-namestring sbcl-home
                               *physical-host*
                               *default-pathname-defaults*
                               :as-directory t))))

(defun user-homedir-namestring (&optional username)
  (flet ((not-empty (x)
           (and (not (equal x "")) x)))
    (if username
        (sb!unix:user-homedir username)
        (or (not-empty (posix-getenv "HOME"))
            #!+win32
            (not-empty (posix-getenv "USERPROFILE"))
            #!+win32
            (let ((drive (not-empty (posix-getenv "HOMEDRIVE")))
                  (path (not-empty (posix-getenv "HOMEPATH"))))
              (and drive path
                   (concatenate 'string drive path)))
            #!-win32
            (not-empty (sb!unix:uid-homedir (sb!unix:unix-getuid)))
            (error "Couldn't find home directory.")))))

;;; (This is an ANSI Common Lisp function.)
(defun user-homedir-pathname (&optional host)
  #!+sb-doc
  "Return the home directory of the user as a pathname. If the HOME
environment variable has been specified, the directory it designates
is returned; otherwise obtains the home directory from the operating
system. HOST argument is ignored by SBCL."
  (declare (ignore host))
  (values
   (parse-native-namestring
    (or (user-homedir-namestring)
        #!+win32
        (sb!win32::get-folder-namestring sb!win32::csidl_profile))
    *physical-host*
    *default-pathname-defaults*
    :as-directory t)))


;;;; DIRECTORY

(defun directory (pathspec &key (resolve-symlinks t))
  #!+sb-doc
  "Return a list of PATHNAMEs, each the TRUENAME of a file that matched the
given pathname. Note that the interaction between this ANSI-specified
TRUENAMEing and the semantics of the Unix filesystem (symbolic links..) means
this function can sometimes return files which don't have the same directory
as PATHNAME. If :RESOLVE-SYMLINKS is NIL, don't resolve symbolic links in
matching filenames."
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
                                   (query-file-system pathname :truename nil)
                                   (query-file-system pathname :existence nil))))
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
    (mapcar #'cdr
            ;; Sorting isn't required by the ANSI spec, but sorting into some
            ;; canonical order seems good just on the grounds that the
            ;; implementation should have repeatable behavior when possible.
            (sort (loop for namestring being each hash-key in truenames
                        using (hash-value truename)
                        collect (cons namestring truename))
                  #'string<
                  :key #'car))))

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
                        (error 'simple-file-error
                               :format-control "Invalid use of ~S after ~S."
                               :format-arguments (list piece (car pieces))
                               :pathname pathname))
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
      (cond ((equal "." name)
             (cond ((not type)
                    (make-pathname :name nil :defaults pathname))
                   ((equal "" type)
                    (make-pathname :name nil
                                   :type nil
                                   :directory (butlast dir)
                                   :defaults pathname))))
            (t
             (make-pathname :name name :type type
                            :directory dir
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
       #!+win32
       (sb!win32::native-call-with-directory-iterator
        #'iterate ,namestring ,errorp)
       #!-win32
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
                (let ((ent (sb!unix:unix-readdir dp nil)))
                  (when ent
                    (let ((name (sb!unix:unix-dirent-name ent)))
                      (when name
                        (cond ((equal "." name)
                               (go :next))
                              ((equal ".." name)
                               (go :next))
                              (t
                               (return-from one-iter name))))))))))
        (unwind-protect
             (progn
               (setf dp (sb!unix:unix-opendir namestring errorp))
               (when dp
                 (funcall function #'one-iter)))
          (when dp
            (sb!unix:unix-closedir dp nil)))))))

;;; This is our core directory access interface that we use to implement
;;; DIRECTORY.
(defun map-directory (function directory &key (files t) (directories t)
                      (classify-symlinks t) (errorp t))
  #!+sb-doc
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
         (realname (query-file-system physical :existence nil))
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
                            (truename (query-file-system tmpname :truename nil)))
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
  (let* ((dir (pathname-directory pathspec))
         (length (length dir))
         (wild (position-if (lambda (elt)
                              (or (eq :wild elt) (typep elt 'pattern)))
                            dir))
         (wild-inferiors (position :wild-inferiors dir))
         (end (cond ((and wild wild-inferiors)
                     (min wild wild-inferiors))
                    (t
                     (or wild wild-inferiors length))))
         (rest (subseq dir end))
         (starting-point (make-pathname :directory (subseq dir 0 end)
                                        :device (pathname-device pathspec)
                                        :host (pathname-host pathspec)
                                        :name nil
                                        :type nil
                                        :version nil)))
    (cond (wild-inferiors
           (map-wild-inferiors function rest starting-point))
          (wild
           (map-wild function rest starting-point))
          (t
           ;; Nothing wild -- the directory matches itself.
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
               (funcall #'cont sub))))
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
           (t (return-from pathname-intersections nil))))
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


(defun directory-pathname-p (pathname)
  (and (pathnamep pathname)
       (null (pathname-name pathname))
       (null (pathname-type pathname))))

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
    (let* ((dir (pathname-directory pathname))
           (*default-pathname-defaults*
             (make-pathname :directory dir :device (pathname-device pathname)))
          (dev (pathname-device pathname)))
      (loop for i from (case dev (:unc 3) (otherwise 2))
              upto (length dir)
            do
            (let* ((newpath (make-pathname
                             :host (pathname-host pathname)
                             :device dev
                             :directory (subseq dir 0 i)))
                   (probed (probe-file newpath)))
              (unless (directory-pathname-p probed)
                (let ((namestring (coerce (native-namestring newpath)
                                          'string)))
                  (when verbose
                    (format *standard-output*
                            "~&creating directory: ~A~%"
                            namestring))
                  (sb!unix:unix-mkdir namestring mode)
                  (unless (directory-pathname-p (probe-file newpath))
                    (restart-case
                        (error
                         'simple-file-error
                         :pathname pathspec
                         :format-control
                         (if (and probed
                                  (not (directory-pathname-p probed)))
                             "Can't create directory ~A,~
                                 ~%a file with the same name already exists."
                             "Can't create directory ~A")
                         :format-arguments (list namestring))
                      (retry ()
                        :report "Retry directory creation."
                        (ensure-directories-exist
                         pathspec
                         :verbose verbose :mode mode))
                      (continue ()
                        :report
                        "Continue as if directory creation was successful."
                        nil)))
                  (setf created-p t)))))
      (values pathspec created-p))))

(/show0 "filesys.lisp 1000")
