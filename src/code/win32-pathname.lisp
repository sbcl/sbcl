;;;; pathname parsing for Win32 filesystems

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun extract-device (namestr start end)
  (declare (type simple-base-string namestr)
           (type index start end))
  (if (and (>= end (+ start 2))
           (alpha-char-p (char namestr start))
           (eql (char namestr (1+ start)) #\:))
      (values (string (char namestr start)) (+ start 2))
      (values nil start)))

(defun split-at-slashes-and-backslashes (namestr start end)
  (declare (type simple-base-string namestr)
           (type index start end))
  (let ((absolute (and (/= start end)
                       (or (char= (schar namestr start) #\/)
                           (char= (schar namestr start) #\\)))))
    (when absolute
      (incf start))
    ;; Next, split the remainder into slash-separated chunks.
    (collect ((pieces))
      (loop
        (let ((slash (position-if (lambda (c)
                                    (or (char= c #\/)
                                        (char= c #\\)))
                                  namestr :start start :end end)))
          (pieces (cons start (or slash end)))
          (unless slash
            (return))
          (setf start (1+ slash))))
      (values absolute (pieces)))))

(defun parse-win32-namestring (namestring start end)
  (declare (type simple-string namestring)
           (type index start end))
  (setf namestring (coerce namestring 'simple-base-string))
  (multiple-value-bind (device new-start)
      (extract-device namestring start end)
    (multiple-value-bind (absolute pieces)
        (split-at-slashes-and-backslashes namestring new-start end)
      (multiple-value-bind (name type version)
          (let* ((tail (car (last pieces)))
                 (tail-start (car tail))
                 (tail-end (cdr tail)))
            (unless (= tail-start tail-end)
              (setf pieces (butlast pieces))
              (extract-name-type-and-version namestring tail-start tail-end)))

        (when (stringp name)
          (let ((position (position-if (lambda (char)
                                         (or (char= char (code-char 0))
                                             (char= char #\/)))
                                       name)))
            (when position
              (error 'namestring-parse-error
                     :complaint "can't embed #\\Nul or #\\/ in Unix namestring"
                     :namestring namestring
                     :offset position))))
        ;; Now we have everything we want. So return it.
        (values nil ; no host for Win32 namestrings
                device
                (collect ((dirs))
                  (dolist (piece pieces)
                    (let ((piece-start (car piece))
                          (piece-end (cdr piece)))
                      (unless (= piece-start piece-end)
                        (cond ((string= namestring ".."
                                        :start1 piece-start
                                        :end1 piece-end)
                               (dirs :up))
                              ((string= namestring "**"
                                        :start1 piece-start
                                        :end1 piece-end)
                               (dirs :wild-inferiors))
                              (t
                               (dirs (maybe-make-pattern namestring
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
                version)))))

(defun parse-native-win32-namestring (namestring start end)
  (declare (type simple-string namestring)
           (type index start end))
  (setf namestring (coerce namestring 'simple-base-string))
  (multiple-value-bind (device new-start)
      (extract-device namestring start end)
    (multiple-value-bind (absolute ranges)
        (split-at-slashes-and-backslashes namestring new-start end)
      (let* ((components (loop for ((start . end) . rest) on ranges
                               for piece = (subseq namestring start end)
                               collect (if (and (string= piece "..") rest)
                                           :up
                                           piece)))
             (name-and-type
              (let* ((end (first (last components)))
                     (dot (position #\. end :from-end t)))
                ;; FIXME: can we get this dot-interpretation knowledge
                ;; from existing code?  EXTRACT-NAME-TYPE-AND-VERSION
                ;; does slightly more work than that.
                (cond
                  ((string= end "")
                   (list nil nil))
                  ((and dot (> dot 0))
                   (list (subseq end 0 dot) (subseq end (1+ dot))))
                  (t
                   (list end nil))))))
        (values nil
                device
                (cons (if absolute :absolute :relative) (butlast components))
                (first name-and-type)
                (second name-and-type)
                nil)))))



(defun unparse-win32-host (pathname)
  (declare (type pathname pathname)
           (ignore pathname))
  ;; FIXME: same as UNPARSE-UNIX-HOST.  That's probably not good.
  "")

(defun unparse-win32-device (pathname)
  (declare (type pathname pathname))
  (let ((device (pathname-device pathname)))
    (if (or (null device) (eq device :unspecific))
        ""
        (concatenate 'simple-string (string device) ":"))))

(defun unparse-win32-piece (thing)
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
              'simple-base-string
              (strings))))))

(defun unparse-win32-directory-list (directory)
  (declare (type list directory))
  (collect ((pieces))
    (when directory
      (ecase (pop directory)
        (:absolute
         (pieces "\\"))
        (:relative
         ;; nothing special
         ))
      (dolist (dir directory)
        (typecase dir
          ((member :up)
           (pieces "..\\"))
          ((member :back)
           (error ":BACK cannot be represented in namestrings."))
          ((member :wild-inferiors)
           (pieces "**\\"))
          ((or simple-string pattern (member :wild))
           (pieces (unparse-unix-piece dir))
           (pieces "\\"))
          (t
           (error "invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-base-string (pieces))))

(defun unparse-win32-directory (pathname)
  (declare (type pathname pathname))
  (unparse-win32-directory-list (%pathname-directory pathname)))

(defun unparse-win32-file (pathname)
  (declare (type pathname pathname))
  (collect ((strings))
    (let* ((name (%pathname-name pathname))
           (type (%pathname-type pathname))
           (type-supplied (not (or (null type) (eq type :unspecific)))))
      ;; Note: by ANSI 19.3.1.1.5, we ignore the version slot when
      ;; translating logical pathnames to a filesystem without
      ;; versions (like Win32).
      (when name
        (when (and (null type)
                   (typep name 'string)
                   (> (length name) 0)
                   (position #\. name :start 1))
          (error "too many dots in the name: ~S" pathname))
        (when (and (typep name 'string)
                   (string= name ""))
          (error "name is of length 0: ~S" pathname))
        (strings (unparse-unix-piece name)))
      (when type-supplied
        (unless name
          (error "cannot specify the type without a file: ~S" pathname))
        (when (typep type 'simple-string)
          (when (position #\. type)
            (error "type component can't have a #\. inside: ~S" pathname)))
        (strings ".")
        (strings (unparse-unix-piece type))))
    (apply #'concatenate 'simple-base-string (strings))))

(defun unparse-win32-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-base-string
               (unparse-win32-device pathname)
               (unparse-win32-directory pathname)
               (unparse-win32-file pathname)))

(defun unparse-native-win32-namestring (pathname)
  (declare (type pathname pathname))
  (let ((device (pathname-device pathname))
        (directory (pathname-directory pathname))
        (name (pathname-name pathname))
        (type (pathname-type pathname)))
    (coerce
     (with-output-to-string (s)
       (when device
         (write-string device s)
         (write-char #\: s))
       (ecase (car directory)
         (:absolute (write-char #\\ s))
         (:relative))
       (dolist (piece (cdr directory))
         (typecase piece
           ((member :up) (write-string ".." s))
           (string (write-string piece s))
           (t (error "ungood piece in NATIVE-NAMESTRING: ~S" piece)))
         (write-char #\\ s))
       (when name
         (unless (stringp name)
           (error "non-STRING name in NATIVE-NAMESTRING: ~S" name))
         (write-string name s)
         (when type
           (unless (stringp type)
             (error "non-STRING type in NATIVE-NAMESTRING: ~S" name))
           (write-char #\. s)
           (write-string type s))))
     'simple-base-string)))

;;; FIXME.
(defun unparse-win32-enough (pathname defaults)
  (declare (type pathname pathname defaults))
  (flet ((lose ()
           (error "~S cannot be represented relative to ~S."
                  pathname defaults)))
    (collect ((strings))
      (let* ((pathname-directory (%pathname-directory pathname))
             (defaults-directory (%pathname-directory defaults))
             (prefix-len (length defaults-directory))
             (result-directory
              (cond ((null pathname-directory) '(:relative))
                    ((eq (car pathname-directory) :relative)
                     pathname-directory)
                    ((and (> prefix-len 1)
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
                     (bug "Bad fallthrough in ~S" 'unparse-unix-enough)))))
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
