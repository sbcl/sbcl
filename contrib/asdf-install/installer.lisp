(in-package :asdf-install)

(defvar *proxy* (posix-getenv "http_proxy"))
(defvar *cclan-mirror*
  (or (posix-getenv "CCLAN_MIRROR")
      "http://ftp.linux.org.uk/pub/lisp/cclan/"))
    
(defun directorify (name)
  ;; input name may or may not have a training #\/, but we know we
  ;; want a directory
  (let ((path (pathname name)))
    (if (pathname-name path)
	(merge-pathnames
	 (make-pathname :directory `(:relative ,(pathname-name path))
			:name "")
	 path)
	path)))

(defvar *sbcl-home* (directorify (posix-getenv "SBCL_HOME")))
(defvar *dot-sbcl*
  (merge-pathnames (make-pathname :directory '(:relative ".sbcl"))
		   (user-homedir-pathname)))

(defvar *verify-gpg-signatures* :unknown-locations
  "Should we get detached GPG signatures for the packages and verify them?
NIL - no, T - yes, :UNKNOWN-LOCATIONS - for any URL which isn't in CCLAN
and doesn't begin with one of the prefixes in *SAFE-URL-PREFIXES*")
(defvar *safe-url-prefixes*
  (list "http://ftp.linux.org.uk/pub/lisp/"
	"http://files.b9.com/"))

(defun verify-gpg-signatures-p (url)
  (labels ((prefixp (prefix string)
	     (let ((m (mismatch prefix string)))
	       (or (not m) (>= m (length prefix))))))
    (case *verify-gpg-signatures*
      (nil nil)
      (:unknown-locations
       (notany
	(lambda (x) (prefixp x url))
	(cons *cclan-mirror* *safe-url-prefixes*)))
      (t t))))
	  
(defvar *locations*
  `((,(merge-pathnames "site/" *sbcl-home*)
     ,(merge-pathnames "site-systems/" *sbcl-home*)
     "System-wide install")
    (,(merge-pathnames "site/" *dot-sbcl*)
     ,(merge-pathnames "systems/" *dot-sbcl*)
     "Personal installation")))

(let* ((*package* (find-package :asdf-install-customize))
       (file (probe-file (merge-pathnames
			  (make-pathname :name ".asdf-install")
			  (user-homedir-pathname)))))
  (when file (load file)))

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
	     
(defun url-host (url)
  (assert (string-equal url "http://" :end1 7))
  (let* ((port-start (position #\: url :start 7))
	 (host-end (min (or (position #\/ url :start 7) (length url))
			(or port-start (length url)))))
    (subseq url 7 host-end)))

(defun url-port (url)
  (assert (string-equal url "http://" :end1 7))
  (let ((port-start (position #\: url :start 7)))
    (if port-start (parse-integer url :start port-start :junk-allowed t) 80)))

(defun url-connection (url)
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp))
	(host (url-host url))
	(port (url-port url)))
    (declare (ignore port))
    (socket-connect
     s (car (host-ent-addresses (get-host-by-name (url-host (or *proxy* url)))))
     (url-port (or  *proxy* url)))
    (let ((stream (socket-make-stream s :input t :output t :buffering :full)))
      ;; we are exceedingly unportable about proper line-endings here.
      ;; Anyone wishing to run this under non-SBCL should take especial care
      (format stream "GET ~A HTTP/1.0~%Host: ~A~%Cookie: CCLAN-SITE=~A~%~%"
	      url host *cclan-mirror*)
      (force-output stream)
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
	(with-open-file (o file-name :direction :output)
	  (if length
	      (let ((buf (make-array length
				     :element-type
				     (stream-element-type stream)  )))
		(read-sequence buf stream)
		(write-sequence buf o)) 
	      (sb-executable:copy-stream stream o))))
      (close stream)
      (terpri)
      ;; seems to have worked.  let's try for a detached gpg signature too
      (when (verify-gpg-signatures-p url)
	(verify-gpg-signature url file-name)))))

(defun verify-gpg-signature (url file-name)
  (destructuring-bind (response headers stream)
      (url-connection (concatenate 'string url ".asc"))
    (unwind-protect
	 (if (= response 200)
	     ;; sadly, we can't pass the stream directly to run-program,
	     ;; because (at least in sbcl 0.8) that ignores existing buffered
	     ;; data and only reads new fresh data direct from the file
	     ;; descriptor
	     (let ((data (make-string (parse-integer
				       (cdr (assoc :content-length headers))
				       :junk-allowed t))))
	       (read-sequence data stream)
	       (let ((ret
		      (process-exit-code
		       (sb-ext:run-program "gpg"
					   (list "--verify" "-"
						 (namestring file-name))
					   :output t
					   :search t
					   :input (make-string-input-stream data)
					   :wait t))))
		 (unless (zerop ret)
		   (error 'signature-error
			  :cause (make-condition
				  'simple-error
				  :format-control "GPG returned exit status ~A"
				  :format-arguments (list ret))))))
	     (error 'signature-error
		    :cause
		    (make-condition
		     'download-error :url  (concatenate 'string url ".asc")
		     :response response)))
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

(defun install-package (source system packagename)
  "Returns a list of asdf system names for installed asdf systems"
  (ensure-directories-exist source )
    (ensure-directories-exist system )
  (let* ((tar
	  (with-output-to-string (o)
	    (or
	     (sb-ext:run-program "tar"
				 (list "-C" (namestring source)
				       "-xzvf" (namestring packagename))
				 :output o
				 :search t
				 :wait t)
	     (error "can't untar"))))
	 (dummy (princ tar))
	 (pos-slash (position #\/ tar))
	 (*default-pathname-defaults*
	  (merge-pathnames
	   (make-pathname :directory
			  `(:relative ,(subseq tar 0 pos-slash)))
	   source)))
    (declare (ignore dummy))
    (loop for asd in (directory
		      (make-pathname :name :wild :type "asd"))
	  do (let ((target (merge-pathnames
			    (make-pathname :name (pathname-name asd)
					   :type (pathname-type asd))
			    system)))
	       (when (probe-file target)
		 (sb-posix:unlink target))
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
  (let ((*temporary-files* nil))
    (unwind-protect
	 (destructuring-bind (source system name) (where)
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
			(handler-case
			    (asdf:operate 'asdf:load-op asd)
			  (asdf:missing-dependency (c)
			    (format t
				    "Downloading package ~A, required by ~A~%"
				    (asdf::missing-requires c)
				    (asdf:component-name
				     (asdf::missing-required-by c)))
			    (one-iter (list
				       (symbol-name
					(asdf::missing-requires c)))))))))
	     (one-iter packages)))
      (dolist (l *temporary-files*)
	    (when (probe-file l) (delete-file l))))))
