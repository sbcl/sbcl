;;;; Convert tabs to spaces and delete trailing whitespace in files.
;;;;
;;;; To be run in the root directory of the distribution as part of
;;;; make.sh.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; Stream and single-file functions

(progn
(defun whitespace-p (character)
  (member character '(#\Space #\Tab) :test #'char=))

(defun canonicalize-whitespace (input output)
  (let (change-p)
    (flet ((remove-trailing-whitespace (line)
             (let ((non-ws-position (position-if-not #'whitespace-p line
                                                     :from-end t)))
               (cond
                 ((not non-ws-position)
                  (unless (zerop (length line))
                    (setq change-p t))
                  "")
                 ((< non-ws-position (1- (length line)))
                  (setq change-p t)
                  (subseq line 0 (1+ non-ws-position)))
                 (t
                  line))))
           (remove-tabs (line)
             (unless (find #\Tab line :test #'char=)
               (return-from remove-tabs line))
             (setq change-p t)
             (with-output-to-string (stream)
               (loop :for char :across line :do
                  (if (char= char #\Tab)
                      (write-string "        " stream)
                      (write-char char stream))))))
      (loop :for line = (read-line input nil :eof)
         :until (eq line :eof)
         :do (let ((clean (remove-tabs (remove-trailing-whitespace line))))
               (write-line clean output))))
    change-p))

(defun canonicalize-whitespace/file (file)
  (macrolet ((with-open-source-file ((stream pathname direction) &body body)
               `(with-open-file (,stream ,pathname
                                         :direction ,direction
                                         :external-format #-clisp :utf-8 #+clisp charset:utf-8)
                  ,@body)))
    (let* ((temporary (make-pathname :type "temp" :defaults file))
           (change-p
            (handler-case
                (with-open-source-file (input file :input)
                  (with-open-source-file (output temporary :output)
                    (canonicalize-whitespace input output)))
              (#+sbcl sb-int:stream-decoding-error #-sbcl error ()
                (format t "// Ignoring non-UTF-8 source file ~S~%" file)
                nil))))
      (cond
        (change-p
         (delete-file file)
         (rename-file temporary file)
         t)
        ((probe-file temporary)
         (delete-file temporary)
         nil)))))

;;; Timestamp functions

(defvar *stamp-file* "whitespace-stamp")

(defun read-stamp-file ()
  (if (probe-file *stamp-file*)
      (file-write-date *stamp-file*)
      0))

(defun write-stamp-file ()
  ;; We want the stamp file to have the current time for its write
  ;; date. Conforming variation in OPEN's IF-EXISTS semantics across
  ;; existing XC hosts means it's simplest to unconditionally ensure a
  ;; new file.
  (when (probe-file *stamp-file*)
    (delete-file *stamp-file*))
  (close (open *stamp-file* :direction :output :if-exists :error
               :if-does-not-exist :create)))

;;; Repository-level functions

(defvar *source-types* '("lisp" "lisp-expr" "c" "h" "asd" "texinfo"))

(defvar *exceptions* '("compile-file-pos-utf16be"))

(defun canonicalize-whitespace/directory
    (&optional (directory *default-pathname-defaults*) (report t))
  (let ((stamp-date (read-stamp-file)) (n-files 0) (n-newer 0) (n-changed 0))
    (labels ((older-than-stamp (file)
               (< (file-write-date file) stamp-date))
             (exception-p (file)
               (member (pathname-name file) *exceptions*
                       :test #'string=))
             (skip-p (file)
               (incf n-files)
               (or (older-than-stamp file) (exception-p file)))
             (directory* (pattern)
               ;; We might be in a build tree made out of symlinks, so
               ;; we should list our files without resolving
               ;; symlinks. (This is implementation-dependent, but
               ;; many implementations on Unix can do it. The list of
               ;; implementations covered below might be larger than
               ;; the set that can build SBCL.)
               (apply #-clisp #'directory
                      #+clisp #'(lambda (pathname &rest keys)
                                  (mapcar #'first
                                          (apply #'directory pathname keys)))
                      pattern
                      #+abcl '(:resolve-symlinks nil)
                      #+allegro '(:follow-symbolic-links nil)
                      #+ccl '(:follow-links nil)
                      #+clisp '(:full t)
                      #+cmu '(:truenamep nil)
                      #+ecl '(:resolve-symlinks nil)
                      #+lispworks '(:link-transparency nil)
                      #+sbcl '(:resolve-symlinks nil)
                      #-(or abcl allegro ccl clisp cmu ecl lispworks sbcl)
                      #.(error "Don't know how to list files without ~
                                resolving symlinks."))))
      (dolist (type *source-types*)
        (let* ((pattern (merge-pathnames
                         (make-pathname :type type
                                        :name :wild
                                        :directory '(:relative :wild-inferiors))
                         directory))
               (files (remove-if #'skip-p (directory* pattern))))
          (incf n-newer (length files))
          (incf n-changed (count-if #'canonicalize-whitespace/file files)))))
    (when report
      (format t "~&// Rewrote ~D of ~D new files out of ~D total."
              n-changed n-newer n-files))
    (write-stamp-file)))
) ; end PROGN

;;; Entry point

(unless (<= cl:char-code-limit 256) (canonicalize-whitespace/directory))
