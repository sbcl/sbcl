;;;; pathname parsing for Unix filesystems

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defstruct (unix-host
             (:copier nil)
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

(setq *physical-host* (make-unix-host))

(defun parse-unix-namestring (namestring start end)
  (declare (type simple-string namestring)
           (type index start end))
  (if (= start end)
      (values nil nil nil nil nil nil)
      (let (absolute
            home
            (first-char (schar namestring start)))
        (cond ((char= first-char #\/)
               (setf absolute t)
               (incf start))
              ((char= first-char #\~)
               (let ((slash (loop for i from start below end
                                  when (char= (char namestring i) #\/)
                                  return i)))
                 (when slash
                   (setf absolute t)
                   (if (> slash (1+ start))
                       (setf home (list :home (subseq namestring (1+ start) slash)))
                       (setf home :home))
                   (setf start slash)))))
        (collect ((dirs))
          (flet ((dir (piece-start piece-end)
                   (unless (= piece-start piece-end)
                     (let ((length (- piece-end piece-start)))
                       (dirs (cond ((and (= length 2)
                                         (char= (char namestring piece-start) #\.)
                                         (char= (char namestring (1+ piece-start)) #\.))
                                    :up)
                                   ((and (= length 2)
                                         (char= (char namestring piece-start) #\*)
                                         (char= (char namestring (1+ piece-start)) #\*))
                                    :wild-inferiors)
                                   (t
                                    (maybe-make-pattern namestring
                                                        piece-start
                                                        piece-end
                                                        #\\))))))))
            (loop for i from start below end
                  for char = (char namestring i)
                  when (char= char #\/)
                  do (dir start i)
                     (setf start (1+ i))))
          (multiple-value-bind  (name type version)
              (unless (= start end)
                (extract-name-type-and-version namestring start end #\\))
            (values nil nil
                    (cond (absolute
                           (if home
                               (list* :absolute home (dirs))
                               (cons :absolute (dirs))))
                          ((dirs)
                           (cons :relative (dirs)))
                          (t
                           nil))
                    name type version))))))

(defun parse-native-unix-namestring (namestring start end as-directory)
  (declare (type simple-string namestring)
           (type index start end))
  (if (= start end)
      (values nil nil nil nil nil nil)
      (let (absolute)
        (cond ((char= (schar namestring start) #\/)
               (setf absolute t)
               (incf start)))
        (collect ((dirs))
          (flet ((dir (piece-start piece-end)
                   (unless (= piece-start piece-end)
                     (let ((length (- piece-end piece-start)))
                       (dirs (cond ((and (= length 2)
                                         (char= (char namestring piece-start) #\.)
                                         (char= (char namestring (1+ piece-start)) #\.))
                                    :up)
                                   (t
                                    (subseq namestring piece-start piece-end))))))))
            (loop for i from start below end
                  for char = (char namestring i)
                  when (char= char #\/)
                  do (dir start i)
                     (setf start (1+ i)))
            (multiple-value-bind (name type)
                (cond (as-directory
                       (dir start end)
                       nil)
                      ((/= start end)
                       (let ((dot (loop for i from (1- end) downto (1+ start)
                                        when (char= (char namestring i) #\.)
                                        return i)))
                         (cond
                           ((and dot (> dot 0))
                            (values (subseq namestring start dot)
                                    (subseq namestring (1+ dot) end)))
                           (t
                            (values (subseq namestring start end) nil))))))
              (values nil nil
                      (cond (absolute
                             (cons :absolute (dirs)))
                            ((dirs)
                             (cons :relative (dirs)))
                            (t
                             nil))
                      name type nil)))))))

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
  (unparse-physical-file pathname #\\))

(defun unparse-unix-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
               (unparse-physical-directory pathname #\\)
               (unparse-physical-file pathname #\\)))

(defun unparse-native-unix-namestring (pathname as-file)
  (declare (type pathname pathname))
  (let ((directory (pathname-directory pathname))
        (seperator-after-directory-p
         (or (pathname-component-present-p (pathname-name pathname))
             (not as-file))))
    (%with-output-to-string (s)
      (when directory
        (ecase (pop directory)
          (:absolute
           (if (typep (car directory) '(or (eql :home) (cons (eql :home))))
               (let* ((home (pop directory))
                      (username (and (consp home) (second home)))
                      (namestring (handler-case
                                      (user-homedir-namestring username)
                                    (error (condition)
                                      (no-native-namestring-error
                                       pathname
                                       "user homedir not known~@[ for ~S~]: ~A"
                                       username condition))))
                      (length (length namestring)))
                 (write-string namestring s)
                 (unless (and (plusp length)
                              (char= (char namestring (1- length)) #\/))
                   (write-char #\/ s)))
               (write-char #\/ s)))
          (:relative)))
      (loop for (piece . subdirs) on directory
            do (typecase piece
                 ((member :up :back)
                  (write-string ".." s))
                 (string
                  (write-string piece s))
                 (t
                  (no-native-namestring-error
                   pathname "of the directory segment ~S." piece)))
            when (or subdirs seperator-after-directory-p)
            do (write-char #\/ s))
      (write-string (unparse-native-physical-file pathname) s))))

(defun unparse-unix-enough (pathname defaults)
  (unparse-physical-enough pathname defaults #\\))

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
