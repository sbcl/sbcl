;;;; pathname parsing for Win32 filesystems

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defstruct (win32-host
             (:include host
                       (parse #'parse-win32-namestring)
                       (parse-native #'parse-native-win32-namestring)
                       (unparse #'unparse-win32-namestring)
                       (unparse-native #'unparse-native-win32-namestring)
                       (unparse-host #'unparse-win32-host)
                       (unparse-directory #'unparse-win32-directory)
                       (unparse-file #'unparse-win32-file)
                       (unparse-enough #'unparse-win32-enough)
                       (unparse-directory-separator "\\")
                       (simplify-namestring #'simplify-win32-namestring)
                       (customary-case :lower))))

(setq *physical-host* (make-win32-host))

;;;
(define-symbol-macro +long-file-name-prefix+ (quote "\\\\?\\"))
(define-symbol-macro +unc-file-name-prefix+ (quote "\\\\?\\UNC"))

(defun extract-device (namestr start end)
  (declare (type simple-string namestr)
           (type index start end))
  (if (>= end (+ start 2))
      (let ((c0 (char namestr start))
            (c1 (char namestr (1+ start))))
        (cond ((and (eql c1 #\:) (alpha-char-p c0))
               ;; "X:" style, saved as X
               (values (string (char namestr start)) (+ start 2)))
              ((and (member c0 '(#\/ #\\)) (eql c0 c1) (>= end (+ start 3)))
               ;; "//UNC" style, saved as :UNC device, with host and share
               ;; becoming directory components.
               (values :unc (+ start 1)))
              (t
               (values nil start))))
      (values nil start)))

(defun split-at-slashes-and-backslashes (namestr start end)
  (declare (type simple-string namestr)
           (type index start end))
  ;; FIXME: There is a fundamental brokenness in using the same
  ;; character as escape character and directory separator in
  ;; non-native pathnames. (PATHNAME-DIRECTORY #P"\\*/") should
  ;; probably be (:RELATIVE "*") everywhere, but on Windows it's
  ;; (:ABSOLUTE :WILD)! See lp#673625.
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
  (setf namestring (coerce namestring 'simple-string))
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
              (extract-name-type-and-version namestring tail-start tail-end #\^)))

        (when (stringp name)
          (let ((position (position-if (lambda (char)
                                         (or (char= char (code-char 0))
                                             (char= char #\/)))
                                       name)))
            (when position
              (error 'namestring-parse-error
                     :complaint "can't embed #\\Nul or #\\/ in Windows namestring"
                     :namestring namestring
                     :offset position))))

        (let (home)
          ;; Deal with ~ and ~user.
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
          (values nil                 ; no host for Win32 namestrings
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
                                                           piece-end
                                                           #\^)))))))
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
                  version))))))

(defun parse-native-win32-namestring (namestring start end as-directory)
  (declare (type simple-string namestring)
           (type index start end))
  (setf namestring (coerce namestring 'simple-string))
  (multiple-value-bind (device new-start)
      (cond ((= (length +unc-file-name-prefix+)
                (mismatch +unc-file-name-prefix+ namestring
                          :start2 start))
             (values :unc (+ start (length +unc-file-name-prefix+))))
            ((= (length +long-file-name-prefix+)
                (mismatch +long-file-name-prefix+ namestring
                          :start2 start))
             (extract-device namestring
                             (+ start (length +long-file-name-prefix+))
                             end))
            (t (extract-device namestring start end)))
    (multiple-value-bind (absolute ranges)
        (split-at-slashes-and-backslashes namestring new-start end)
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
                device
                (cond (absolute
                       (cons :absolute directory))
                      (directory
                       (cons :relative directory))
                      (as-directory
                       '(:absolute)))
                (first name-and-type)
                (second name-and-type)
                nil)))))

(defun unparse-win32-host (pathname)
  (declare (type pathname pathname)
           (ignore pathname))
  ;; FIXME: same as UNPARSE-UNIX-HOST.  That's probably not good.
  "")

(defun unparse-win32-device (pathname &optional native)
  (declare (type pathname pathname))
  (let ((device (pathname-device pathname))
        (directory (pathname-directory pathname)))
    (cond ((not (pathname-component-present-p device))
           "")
          ((eq device :unc)
           (if native "\\" "/"))
          ((and (= 1 (length device)) (alpha-char-p (char device 0)))
           (concatenate 'simple-string device ":"))
          ((and (consp directory) (eq :relative (car directory)))
           (no-native-namestring-error
            pathname
            "there is no printed representation for a relative UNC pathname"))
          (t
           (if native
               (concatenate 'simple-string "\\\\" device)
               (concatenate 'simple-string "//" device))))))

(defun unparse-win32-directory (pathname)
  (unparse-physical-directory pathname #\^))

(defun unparse-win32-file (pathname)
  (declare (type pathname pathname))
  (unparse-physical-file pathname #\^))

(defun unparse-win32-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
               (unparse-win32-device pathname)
               (unparse-physical-directory pathname #\^)
               (unparse-physical-file pathname #\^)))

(defun unparse-native-win32-namestring (pathname as-file)
  (declare (type pathname pathname))
  (let* ((device (pathname-device pathname))
         (devicep (not (member device '(:unc nil))))
         (directory (pathname-directory pathname))
         (absolutep (and device (eql :absolute (car directory))))
         (seperator-after-directory-p
          (or (pathname-component-present-p (pathname-name pathname))
              (not as-file))))
    (when (and absolutep (member :up directory))
      ;; employ merge-pathnames to parse :BACKs into which we turn :UPs
      (setf directory
            (pathname-directory
             (merge-pathnames
              (make-pathname :defaults pathname :directory '(:relative))
              (make-pathname :defaults pathname
                             :directory (substitute :back :up directory))))))
    (coerce
     (%with-output-to-string (s)
       (when absolutep
         (write-string (case device
                         (:unc +unc-file-name-prefix+)
                         (otherwise +long-file-name-prefix+))
                       s))
       (when (or (not absolutep) devicep)
         (write-string (unparse-win32-device pathname t) s))
       (when directory
         (ecase (pop directory)
           (:absolute
            (let ((next (pop directory)))
              (cond
                ((typep next '(or (eql :home) (cons (eql :home))))
                 (let* ((username (when (consp next) (second next)))
                        (home (handler-case
                                  (if username
                                      (parse-native-namestring
                                       (user-homedir-namestring username))
                                      (user-homedir-pathname))
                                (error (condition)
                                  (no-native-namestring-error
                                   pathname
                                   "user homedir not known~@[ for ~S~]: ~A"
                                   username condition)))))
                   (when (and (or absolutep devicep)
                              (not (string-equal device (pathname-device home))))
                     (no-native-namestring-error
                      pathname
                      "Device in homedir ~S conflicts which device ~S"
                      home device))
                   (write-string (native-namestring home) s)))
                ;; namestring of user-homedir-pathname already has
                ;; // at the end
                (next
                 (write-char #\\ s)
                 (push next directory))
                (t
                 (write-char #\\ s)))))
           (:relative)))
       (loop for (piece . subdirs) on directory
             do (typecase piece
                  ((member :up :back) (write-string ".." s))
                  (string (write-string piece s))
                  (t (no-native-namestring-error pathname
                                                 "Bad directory segment in NATIVE-NAMESTRING: ~S."
                                                 piece)))
             when (or subdirs seperator-after-directory-p)
             do (write-char #\\ s))
       (write-string (unparse-native-physical-file pathname) s)
       (when absolutep
         (let ((string (get-output-stream-string s)))
           (return-from unparse-native-win32-namestring
             (cond ((< (- 260 12) (length string))
                    ;; KLUDGE: account for additional length of 8.3 name to make
                    ;; directories always accessible
                    (coerce string 'simple-string))
                   ((eq :unc device)
                    (replace
                     (subseq string (1- (length +unc-file-name-prefix+)))
                     "\\"))
                   (t (subseq string (length +long-file-name-prefix+))))))))
     'simple-string)))

(defun unparse-win32-enough (pathname defaults)
  (unparse-physical-enough pathname defaults #\^))

;; FIXME: This has been converted rather blindly from the Unix
;; version, with no reference to any Windows docs what so ever.
(defun simplify-win32-namestring (src)
  (declare (type simple-string src))
  (let* ((src-len (length src))
         (dst (make-string src-len :element-type 'character))
         (dst-len 0)
         (dots 0)
         (last-slash nil))
    (flet ((deposit (char)
             (setf (schar dst dst-len) char)
             (incf dst-len))
           (slashp (char)
             (find char "\\/")))
      (dotimes (src-index src-len)
        (let ((char (schar src src-index)))
          (cond ((char= char #\.)
                 (when dots
                   (incf dots))
                 (deposit char))
                ((slashp char)
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
                              (position-if #'slashp dst :end last-slash :from-end t)))
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
                 (incf dst-len)))))
      ;; ...finish off
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
                            (slashp (schar dst (- last-slash 3)))))
             (let ((prev-prev-slash
                    (position-if #'slashp dst :end last-slash :from-end t)))
               (if prev-prev-slash
                   (setf dst-len (1+ prev-prev-slash))
                   (return-from simplify-win32-namestring
                     (coerce ".\\" 'simple-string)))))))))
    (cond ((zerop dst-len)
           ".\\")
          ((= dst-len src-len)
           dst)
          (t
           (subseq dst 0 dst-len)))))
