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

#+ecl (ext:quit) ; avoids 'Unexpected end of file on #<input file "stdin">.'

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
                                         :if-exists :supersede
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
               ;; we should list our files without resolving symlinks.
               (directory pattern :resolve-symlinks nil)))
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
(values)
) ; end PROGN

;;; Entry point

;;; Bearing in mind that sometimes a developer skips the canonicalizing
;;; step, there are least all these reasons to require SBCL to do it:
;;; 1. SBCL committers must ensure that SBCL built at each commit can
;;;    self-rebuild at that commit. Therefore committers must - in theory -
;;;    execute canonicalize-whitespace as _some_ part of their build.
;;; 2. Most other lisps are slower than SBCL. There's no reason to wait
;;;    half a minute for a build even to start on a snail-speed lisp.
;;; 3. You can't run the build comparison test at a whitespace-uncanonical
;;;    commit because the source file mod times will change.
;;; 4. Users of other lisps who want to try SBCL should not find themselves
;;;    in the position of having to ask why a just-downloaded and built
;;;    SBCL touched its own sources files.
;;; 5. It's impolite that 'git bisect' could touch files in the 'make' step.
;;;    Confining such behavior to the fewest number of users is therefore
;;;    strictly more polite.
;;; 6. An "unknown" host lisp should have exited with success instead of erring
;;;    out. It's got nothing to do with whether the build would succeed.
;;; 7. Far fewer lines of #+/- to maintain in this file.

#+sbcl (unless (<= cl:char-code-limit 256) (canonicalize-whitespace/directory))
