;;;; pathname parsing for Unix filesystems

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defstruct (unix-host
             (:include host
                       (parse #'parse-unix-namestring)
                       (parse-native #'parse-native-unix-namestring)
                       (unparse #'unparse-unix-namestring)
                       (unparse-native #'unparse-native-unix-namestring)
                       (unparse-host #'unparse-unix-host)
                       (unparse-directory #'unparse-unix-directory)
                       (unparse-file #'unparse-unix-file)
                       (unparse-enough #'unparse-unix-enough)
                       (unparse-directory-separator "/")
                       (simplify-namestring #'simplify-unix-namestring)
                       (customary-case :lower))))

(defvar *physical-host* (make-unix-host))

;;; Take a string and return a list of cons cells that mark the char
;;; separated subseq. The first value is true if absolute directories
;;; location.
(defun split-at-slashes (namestr start end)
  (declare (type simple-string namestr)
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

(defun parse-unix-namestring (namestring start end)
  (declare (type simple-string namestring)
           (type index start end))
  (setf namestring (coerce namestring 'simple-string))
  (multiple-value-bind (absolute pieces)
      (split-at-slashes namestring start end)
    (multiple-value-bind (name type version)
        (let* ((tail (car (last pieces)))
               (tail-start (car tail))
               (tail-end (cdr tail)))
          (unless (= tail-start tail-end)
            (setf pieces (butlast pieces))
            (extract-name-type-and-version namestring tail-start tail-end #\\)))

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

      (let (home)
        ;; Deal with ~ and ~user
        (when (car pieces)
          (destructuring-bind (start . end) (car pieces)
            (when (and (not absolute)
                       (not (eql start end))
                       (string= namestring "~"
                                :start1 start
                                :end1 (1+ start)))
              (setf absolute t)
              (if (> end (1+ start))
                  (setf home (list :home (subseq namestring (1+ start) end)))
                  (setf home :home))
              (pop pieces))))

        ;; Now we have everything we want. So return it.
        (values nil                  ; no host for Unix namestrings
                nil                  ; no device for Unix namestrings
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
                                                         piece-end
                                                         #\\)))))))
                  (cond (absolute
                         (if home
                             (list* :absolute home (dirs))
                             (cons :absolute (dirs))))
                        ((dirs)
                         (cons :relative (dirs)))
                        (t
                         nil)))
                name
                type
                version)))))

(defun parse-native-unix-namestring (namestring start end as-directory)
  (declare (type simple-string namestring)
           (type index start end))
  (setf namestring (coerce namestring 'simple-string))
  (multiple-value-bind (absolute ranges)
      (split-at-slashes namestring start end)
    (let* ((components (loop for ((start . end) . rest) on ranges
                             for piece = (subseq namestring start end)
                             collect (if (and (string= piece "..") rest)
                                         :up
                                         piece)))
           (directory (if (and as-directory
                               (string/= "" (car (last components))))
                          components
                          (butlast components)))
           (name-and-type
            (unless as-directory
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
                   (list end nil)))))))
      (values nil
              nil
              (cons (if absolute :absolute :relative) directory)
              (first name-and-type)
              (second name-and-type)
              nil))))

(/show0 "filesys.lisp 300")

(defun unparse-unix-host (pathname)
  (declare (type pathname pathname)
           (ignore pathname))
  ;; this host designator needs to be recognized as a physical host in
  ;; PARSE-NAMESTRING. Until sbcl-0.7.3.x, we had "Unix" here, but
  ;; that's a valid Logical Hostname, so that's a bad choice. -- CSR,
  ;; 2002-05-09
  "")

(defun unparse-unix-directory (pathname)
  (unparse-physical-directory pathname #\\))

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
        (when (and (null type)
                   (typep name 'string)
                   (> (length name) 0)
                   (position #\. name :start 1))
          (error "too many dots in the name: ~S" pathname))
        (when (and (typep name 'string)
                   (string= name ""))
          (error "name is of length 0: ~S" pathname))
        (strings (unparse-physical-piece name #\\)))
      (when type-supplied
        (unless name
          (error "cannot specify the type without a file: ~S" pathname))
        (when (typep type 'simple-string)
          (when (position #\. type)
            (error "type component can't have a #\. inside: ~S" pathname)))
        (strings ".")
        (strings (unparse-physical-piece type #\\))))
    (apply #'concatenate 'simple-string (strings))))

(/show0 "filesys.lisp 406")

(defun unparse-unix-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
               (unparse-unix-directory pathname)
               (unparse-unix-file pathname)))

(defun unparse-native-unix-namestring (pathname as-file)
  (declare (type pathname pathname))
  (let* ((directory (pathname-directory pathname))
         (name (pathname-name pathname))
         (name-present-p (typep name '(not (member nil :unspecific))))
         (name-string (if name-present-p name ""))
         (type (pathname-type pathname))
         (type-present-p (typep type '(not (member nil :unspecific))))
         (type-string (if type-present-p type "")))
    (when name-present-p
      (setf as-file nil))
    (with-simple-output-to-string (s)
      (when directory
        (ecase (pop directory)
          (:absolute
           (let ((next (pop directory)))
             (cond ((eq :home next)
                    (write-string (user-homedir-namestring) s))
                   ((and (consp next) (eq :home (car next)))
                    (let ((where (user-homedir-namestring (second next))))
                      (if where
                          (write-string where s)
                          (error "User homedir unknown for: ~S." (second next)))))
                   (next
                    (push next directory)))
             (write-char #\/ s)))
          (:relative)))
      (loop for (piece . subdirs) on directory
            do (typecase piece
                 ((member :up :back)
                  (write-string ".." s))
                 (string
                  (write-string piece s))
                 (t
                  (error "Bad directory segment in NATIVE-NAMESTRING: ~S."
                         piece)))
            if (or subdirs (stringp name))
            do (write-char #\/ s)
            else
            do (unless as-file
                 (write-char #\/ s)))
      (if name-present-p
          (progn
            (unless (stringp name-string) ;some kind of wild field
              (error "Bad name component in NATIVE-NAMESTRING: ~S." name))
            (write-string name-string s)
            (when type-present-p
              (unless (stringp type-string) ;some kind of wild field
                (error "Bad type component in NATIVE-NAMESTRING: ~S." type))
              (write-char #\. s)
              (write-string type-string s)))
          (when type-present-p          ; type without a name
            (error
             "Type component without a name component in NATIVE-NAMESTRING: ~S."
             type))))))

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
              (cond ((null pathname-directory) '(:relative))
                    ((eq (car pathname-directory) :relative)
                     pathname-directory)
                    ((and (> prefix-len 0)
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
        (strings (unparse-physical-directory-list result-directory #\\)))
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
                     (typep pathname-name 'simple-string)
                     (position #\. pathname-name :start 1))
            (error "too many dots in the name: ~S" pathname))
          (strings (unparse-physical-piece pathname-name #\\)))
        (when type-needed
          (when (or (null pathname-type) (eq pathname-type :unspecific))
            (lose))
          (when (typep pathname-type 'simple-string)
            (when (position #\. pathname-type)
              (error "type component can't have a #\. inside: ~S" pathname)))
          (strings ".")
          (strings (unparse-physical-piece pathname-type #\\))))
      (apply #'concatenate 'simple-string (strings)))))

(defun simplify-unix-namestring (src)
  (declare (type simple-string src))
  (let* ((src-len (length src))
         (dst (make-string src-len :element-type 'character))
         (dst-len 0)
         (dots 0)
         (last-slash nil))
    (macrolet ((deposit (char)
                 `(progn
                    (setf (schar dst dst-len) ,char)
                    (incf dst-len))))
      (dotimes (src-index src-len)
        (let ((char (schar src src-index)))
          (cond ((char= char #\.)
                 (when dots
                   (incf dots))
                 (deposit char))
                ((char= char #\/)
                 (case dots
                   (0
                    ;; either ``/...' or ``...//...'
                    (unless last-slash
                      (setf last-slash dst-len)
                      (deposit char)))
                   (1
                    ;; either ``./...'' or ``..././...''
                    (decf dst-len))
                   (2
                    ;; We've found ..
                    (cond
                      ((and last-slash (not (zerop last-slash)))
                       ;; There is something before this ..
                       (let ((prev-prev-slash
                              (position #\/ dst :end last-slash :from-end t)))
                         (cond ((and (= (+ (or prev-prev-slash 0) 2)
                                        last-slash)
                                     (char= (schar dst (- last-slash 2)) #\.)
                                     (char= (schar dst (1- last-slash)) #\.))
                                ;; The something before this .. is another ..
                                (deposit char)
                                (setf last-slash dst-len))
                               (t
                                ;; The something is some directory or other.
                                (setf dst-len
                                      (if prev-prev-slash
                                          (1+ prev-prev-slash)
                                          0))
                                (setf last-slash prev-prev-slash)))))
                      (t
                       ;; There is nothing before this .., so we need to keep it
                       (setf last-slash dst-len)
                       (deposit char))))
                   (t
                    ;; something other than a dot between slashes
                    (setf last-slash dst-len)
                    (deposit char)))
                 (setf dots 0))
                (t
                 (setf dots nil)
                 (setf (schar dst dst-len) char)
                 (incf dst-len))))))
    (when (and last-slash (not (zerop last-slash)))
      (case dots
        (1
         ;; We've got  ``foobar/.''
         (decf dst-len))
        (2
         ;; We've got ``foobar/..''
         (unless (and (>= last-slash 2)
                      (char= (schar dst (1- last-slash)) #\.)
                      (char= (schar dst (- last-slash 2)) #\.)
                      (or (= last-slash 2)
                          (char= (schar dst (- last-slash 3)) #\/)))
           (let ((prev-prev-slash
                  (position #\/ dst :end last-slash :from-end t)))
             (if prev-prev-slash
                 (setf dst-len (1+ prev-prev-slash))
                 (return-from simplify-unix-namestring
                   (coerce "./" 'simple-string))))))))
    (cond ((zerop dst-len)
           "./")
          ((= dst-len src-len)
           dst)
          (t
           (subseq dst 0 dst-len)))))
