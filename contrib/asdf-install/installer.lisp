(in-package :asdf-install)

(defvar *proxy* (posix-getenv "http_proxy"))
(defvar *cclan-mirror*
  (let ((mirror (posix-getenv "CCLAN_MIRROR")))
    (or (and (not (string= mirror "")) mirror)
        "http://ftp.linux.org.uk/pub/lisp/cclan/")))

(defun directorify (name)
  ;; input name may or may not have a training #\/, but we know we
  ;; want a directory
  (let ((path (pathname name)))
    (if (pathname-name path)
        (merge-pathnames
         (make-pathname :directory `(:relative ,(pathname-name path)))
         (make-pathname :directory (pathname-directory path)
                        :host (pathname-host path)
                        :device (pathname-device path)))
        path)))

(defvar *sbcl-home* (directorify (posix-getenv "SBCL_HOME")))
(defvar *dot-sbcl*
  (merge-pathnames (make-pathname :directory '(:relative ".sbcl"))
                   (user-homedir-pathname)))

(defparameter *trusted-uids* nil)

(defvar *locations*
  `((,(merge-pathnames "site/" *sbcl-home*)
     ,(merge-pathnames "site-systems/" *sbcl-home*)
     "System-wide install")
    (,(merge-pathnames "site/" *dot-sbcl*)
     ,(merge-pathnames "systems/" *dot-sbcl*)
     "Personal installation")))

(unless (sb-ext:posix-getenv "SBCL_BUILDING_CONTRIB")
  ;; Not during build, thanks.
  (let* ((*package* (find-package :asdf-install-customize))
         (file (probe-file (merge-pathnames
                            (make-pathname :name ".asdf-install")
                            (user-homedir-pathname)))))
    (when file (load file))))

(define-condition download-error (error)
  ((url :initarg :url :reader download-url)
   (response :initarg :response :reader download-response))
  (:report (lambda (c s)
             (format s "Server responded ~A for GET ~A"
                     (download-response c) (download-url c)))))

(define-condition signature-error (error)
  ((cause :initarg :cause :reader signature-error-cause))
  (:report (lambda (c s)
             (format s "Cannot verify package signature:  ~A"
                     (signature-error-cause c)))))

(define-condition gpg-error (error)
  ((message :initarg :message :reader gpg-error-message))
  (:report (lambda (c s)
             (format s "GPG failed with error status:~%~S"
                     (gpg-error-message c)))))

(define-condition no-signature (gpg-error) ())
(define-condition key-not-found (gpg-error)
  ((key-id :initarg :key-id :reader key-id))
  (:report (lambda (c s)
             (format s "No key found for key id 0x~A.  Try some command like ~%  gpg  --recv-keys 0x~A"
                     (key-id c) (key-id c)))))

(define-condition key-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
             (format s "GPG warns that the key id 0x~A (~A) is not fully trusted"
                     (key-id c) (key-user-name c)))))
(define-condition author-not-trusted (gpg-error)
  ((key-id :initarg :key-id :reader key-id)
   (key-user-name :initarg :key-user-name :reader key-user-name))
  (:report (lambda (c s)
             (format s "~A (key id ~A) is not on your package supplier list"
                     (key-user-name c) (key-id c)))))

(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
         (host-end (min (or (position #\/ url :start 7) (length url))
                        (or port-start (length url)))))
    (subseq url 7 host-end)))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((port-start (position #\: url :start 7)))
    (if port-start (parse-integer url :start (1+ port-start) :junk-allowed t) 80)))

(defun request-uri (url)
  (assert (string-equal url "http://" :end1 7))
  (if *proxy*
      url
      (let ((path-start (position #\/ url :start 7)))
        (subseq url path-start))))

(defun url-connection (url)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
        (host (url-host url))
        (port (url-port url))
        result)
    (declare (ignore port))
    (unwind-protect
        (progn
          (socket-connect
           s (car (host-ent-addresses (get-host-by-name (url-host (or *proxy* url)))))
           (url-port (or  *proxy* url)))
          (let ((stream (socket-make-stream s :input t :output t :buffering :full
                                            :element-type :default :external-format :iso-8859-1)))
            ;; we are exceedingly unportable about proper line-endings here.
            ;; Anyone wishing to run this under non-SBCL should take especial care
            (format stream "GET ~A HTTP/1.0~c~%~
                            Host: ~A~c~%~
                            Cookie: CCLAN-SITE=~A~c~%~c~%"
                    (request-uri url) #\Return
                    host #\Return
                    *cclan-mirror* #\Return #\Return)
            (force-output stream)
            (setf result
                  (list
                   (let* ((l (read-line stream))
                          (space (position #\Space l)))
                     (parse-integer l :start (1+ space) :junk-allowed t))
                   (loop for line = (read-line stream nil nil)
                         until (or (null line) (eql (elt line 0) (code-char 13)))
                         collect
                         (let ((colon (position #\: line)))
                           (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                                 (string-trim (list #\Space (code-char 13))
                                              (subseq line (1+ colon))))))
                   stream))))
      (when (and (null result)
                 (socket-open-p s))
        (socket-close s)))))


(defun copy-stream (in out)
  (let ((buf (make-array 8192 :element-type (stream-element-type out))))
    (loop for pos = (read-sequence buf in)
          until (zerop pos)
          do (write-sequence buf out :end pos))))

(defun download-files-for-package (package-name-or-url file-name)
  (let ((url
         (if (= (mismatch package-name-or-url "http://") 7)
             package-name-or-url
             (format nil "http://www.cliki.net/~A?download"
                     package-name-or-url))))
    (destructuring-bind (response headers stream)
        (block got
          (loop
           (destructuring-bind (response headers stream) (url-connection url)
             (unless (member response '(301 302))
               (return-from got (list response headers stream)))
             (close stream)
             (setf url (cdr (assoc :location headers))))))
      (if (>= response 400)
        (error 'download-error :url url :response response))
      (let ((length (parse-integer
                     (or (cdr (assoc :content-length headers)) "")
                     :junk-allowed t)))
        (format t "Downloading ~A bytes from ~A ..."
                (if length length "some unknown number of") url)
        (force-output)
        (with-open-file (out file-name :direction :output
                             :element-type '(unsigned-byte 8))
          (if length
              (let ((buf (make-array length :element-type '(unsigned-byte 8))))
                (read-sequence buf stream)
                (write-sequence buf out))
              (copy-stream stream out))))
      (close stream)
      (terpri)
      (restart-case
          (verify-gpg-signature/url url file-name)
        (skip-gpg-check ()
          :report "Don't check GPG signature for this package"
          nil)))))

(defun read-until-eof (stream)
  (with-output-to-string (o)
    (copy-stream stream o)))

(defun verify-gpg-signature/string (string file-name)
  (let* ((proc
          (sb-ext:run-program
           "gpg"
           (list
            "--status-fd" "1" "--verify" "-"
            (namestring file-name))
           :output :stream :error :stream :search t
           :input (make-string-input-stream string) :wait t))
         (err (read-until-eof (process-error proc)))
         tags)
    (loop for l = (read-line (process-output proc) nil nil)
          while l
          when (> (mismatch l "[GNUPG:]") 6)
          do (destructuring-bind (_ tag &rest data) (asdf::split-string l)
               (declare (ignore _))
               (pushnew (cons (intern tag :keyword)
                              data) tags)))
    ;; test for obvious key/sig problems
    (let ((errsig (assoc :errsig tags)))
      (and errsig (error 'key-not-found :key-id (second errsig) :gpg-err err)))
    (let ((badsig (assoc :badsig tags)))
      (and badsig (error 'key-not-found :key-id (second badsig) :gpg-err err)))
    (let* ((good (assoc :goodsig tags))
           (id (second good))
           (name (format nil "~{~A~^ ~}" (nthcdr 2 good))))
      ;; good signature, but perhaps not trusted
      (unless (or (assoc :trust_ultimate tags)
                  (assoc :trust_fully tags))
        (cerror "Install the package anyway"
                'key-not-trusted
                :key-user-name name
                :key-id id :gpg-err err))
      (loop
       (when
           (restart-case
               (or (assoc id *trusted-uids* :test #'equal)
                   (error 'author-not-trusted
                          :key-user-name name
                          :key-id id :gpg-err nil))
             (add-key ()
               :report "Add to package supplier list"
               (pushnew (list id name) *trusted-uids*)))
         (return))))))



(defun verify-gpg-signature/url (url file-name)
  (destructuring-bind (response headers stream)
      (url-connection (concatenate 'string url ".asc"))
    (unwind-protect
         (if (= response 200)
             (let ((data (make-string (parse-integer
                                       (cdr (assoc :content-length headers))
                                       :junk-allowed t))))
               (read-sequence data stream)
               (verify-gpg-signature/string data file-name))
             (error 'download-error :url  (concatenate 'string url ".asc")
                    :response response))
      (close stream))))

(defun where ()
  (format t "Install where?~%")
  (loop for (source system name) in *locations*
        for i from 1
        do (format t "~A) ~A: ~%   System in ~A~%   Files in ~A ~%"
                   i name system source))
  (format t " --> ") (force-output)
  (let ((response (read)))
    (when (> response 0)
      (elt *locations* (1- response)))))

(defparameter *tar-program*
  ;; Please do not "clean this up" by using a bunch of #+'s and one
  ;; #-. When the conditional is written this way, adding a new
  ;; special case only involves one change. If #- is used, two changes
  ;; are needed. -- JES, 2007-02-12
  (progn
    "tar"
    #+darwin "gnutar"
    #+(or sunos netbsd) "gtar"))

(defun get-tar-directory (packagename)
  (let* ((tar (with-output-to-string (o)
                (or
                 (sb-ext:run-program *tar-program*
                                     (list "-tzf" (namestring packagename))
                                     :output o
                                     :search t
                                     :wait t)
                 (error "can't list archive"))))
         (first-line (subseq tar 0 (position #\newline tar))))
    (if (find #\/ first-line)
        (subseq first-line 0 (position #\/ first-line))
        first-line)))

(defun untar-package (source packagename)
  (with-output-to-string (o)
    (or
     (sb-ext:run-program *tar-program*
                         (list "-C" (namestring source)
                               "-xzvf" (namestring packagename))
                         :output o
                         :search t
                         :wait t)
     (error "can't untar"))))

(defun install-package (source system packagename)
  "Returns a list of asdf system names for installed asdf systems"
  (ensure-directories-exist source)
  (ensure-directories-exist system)
  (let* ((tdir (get-tar-directory packagename))
         (*default-pathname-defaults*
          (merge-pathnames (make-pathname :directory `(:relative ,tdir))
                           source)))
    (princ (untar-package source packagename))
    (loop for asd in (directory
                      (make-pathname :name :wild :type "asd"))
          do (let ((target (merge-pathnames
                            (make-pathname :name (pathname-name asd)
                                           :type (pathname-type asd))
                            system)))
               (when (probe-file target)
                 (sb-posix:unlink target))
               #-win32
               (sb-posix:symlink asd target))
          collect (pathname-name asd))))

(defvar *temporary-files*)
(defun temp-file-name (p)
  (let* ((pos-slash (position #\/ p :from-end t))
         (pos-dot (position #\. p :start (or pos-slash 0))))
    (merge-pathnames
     (make-pathname
      :name (subseq p (if pos-slash (1+ pos-slash) 0) pos-dot)
      :type "asdf-install-tmp"))))


;; this is the external entry point
(defun install (&rest packages)
  (let ((*temporary-files* nil)
        (*trusted-uids*
         (let ((p (merge-pathnames "trusted-uids.lisp" *dot-sbcl*)))
           (when (probe-file p)
             (with-open-file (f p) (read f))))))
    (unwind-protect
         (destructuring-bind (source system name) (where)
           (declare (ignore name))
           (labels ((one-iter (packages)
                      (dolist (asd
                                (loop for p in (mapcar 'string packages)
                                      unless (probe-file p)
                                      do (let ((tmp (temp-file-name p)))
                                           (pushnew tmp *temporary-files*)
                                           (download-files-for-package p tmp)
                                           (setf p tmp))
                                      end
                                      do (format t "Installing ~A in ~A,~A~%"
                                                 p source system)
                                      append (install-package source system p)))
                        (handler-bind
                            ((asdf:missing-dependency
                              (lambda (c)
                                (format t
                                        "Downloading package ~A, required by ~A~%"
                                        (asdf::missing-requires c)
                                        (asdf:component-name
                                         (asdf::missing-required-by c)))
                                (one-iter (list
                                           (symbol-name
                                            (asdf::missing-requires c))))
                                (invoke-restart 'retry))))
                          (loop
                           (multiple-value-bind (ret restart-p)
                               (with-simple-restart
                                   (retry "Retry installation")
                                 (asdf:operate 'asdf:load-op asd))
                             (declare (ignore ret))
                             (unless restart-p (return))))))))
             (one-iter packages)))
      (let ((p (merge-pathnames "trusted-uids.lisp" *dot-sbcl*)))
        (ensure-directories-exist p)
        (with-open-file (out p :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (prin1 *trusted-uids* out))))
      (dolist (l *temporary-files*)
        (when (probe-file l) (delete-file l))))))

(defun uninstall (system &optional (prompt t))
  (let* ((asd (asdf:system-definition-pathname system))
         (system (asdf:find-system system))
         (dir (asdf::pathname-sans-name+type
               (asdf::resolve-symlinks asd))))
    (when (or (not prompt)
              (y-or-n-p
               "Delete system ~A~%asd file: ~A~%sources: ~A~%Are you sure?"
               system asd dir))
      (delete-file asd)
      (asdf:run-shell-command "rm -r ~A" (namestring dir)))))

;;; some day we will also do UPGRADE, but we need to sort out version
;;; numbering a bit better first
