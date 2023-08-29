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


#+ecl (ext:quit) ; avoids 'Unexpected end of file on #<input file "stdin">.'

;; The last available build of SBCL for PowerPC is 1.0.47
;; and sb-ext:map-directory were introduced at 1.1.5
#+sbcl
(if (not (apropos-list "map-directory" 'sb-ext))
  (quit))

#+sbcl
(progn

(defvar *buffer* (make-string (* 3/2 1024 1024)))

(defun canonicalize-whitespace (string length)
  (declare ((simple-array character (*)) string))
  (let ((i 0)
        whitespace)
    (loop while (< i length)
          for char = (schar string i)
          do
          (case char
            (#\Newline
             (when whitespace
               (return)))
            (#\Tab
             (unless whitespace
               (setf whitespace i))
             (return))
            (#\Space
             (unless whitespace
               (setf whitespace i)))
            (t
             (setf whitespace nil)))
          (incf i))
    (unless (= i length)
      (with-output-to-string (out)
        (write-string string out :end whitespace)
        (loop for i from whitespace below length
              for char = (schar string i)
              do
              (case char
                (#\Newline
                 (setf whitespace nil)
                 (write-char char out))
                ((#\Space #\Tab)
                 (unless whitespace
                   (setf whitespace i)))
                (t
                 (when whitespace
                   (loop for i from whitespace below i
                         for char = (char string i)
                         do (if (char= char #\Tab)
                                (write-string "        " out)
                                (write-char char out)))
                   (setf whitespace nil))
                 (write-char char out))))))))

(defun canonicalize-whitespace/file (file)
  (let ((new
          (with-open-file (stream file :external-format :utf-8 :if-does-not-exist nil)
            (when stream
              (let ((length (file-length stream)))
                (when (> length (length *buffer*))
                  (when (> length (* 10 1024 1024))
                    (warn "~a too large" file))
                  (setf *buffer* (make-string length))))
              (canonicalize-whitespace *buffer* (read-sequence *buffer* stream))))))
    (when new
      (with-open-file (stream file :direction :output
                                   :external-format :utf-8
                                   :if-exists :supersede)
        (print file)
        (write-sequence new stream)))))

(defvar *source-types* '("lisp" "lisp-expr" "c" "h" "asd" "texinfo"))

(defvar *exceptions* '("compile-file-pos-utf16be"))

(defun find-files ()
  (let (files)
    (labels ((rec (directory)
               (sb-ext:map-directory
                (lambda (file)
                  (if (not (or (pathname-name file)
                               (pathname-type file)))
                      (unless (member (car (last (pathname-directory file))) '(".git" "ansi-test" "obj" "output") :test #'equal)
                        (rec file))
                      (when (and (member (pathname-type file) *source-types* :test #'equal)
                                 (not (member (pathname-name file) *exceptions* :test #'string=)))
                        (push file files))))
                directory
                :classify-symlinks nil)))
      (rec *default-pathname-defaults*))
    files))

(defun canonicalize-whitespace/directory (&key (report t))
  (let* ((files (find-files))
         (n-files (length files))
         (n-changed 0))
    (incf n-changed (count-if #'canonicalize-whitespace/file files))
    (when report
      (format t "~&// Rewrote ~D files out of ~D."
              n-changed n-files))))

(map nil #'compile '(find-files canonicalize-whitespace/file canonicalize-whitespace))

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
