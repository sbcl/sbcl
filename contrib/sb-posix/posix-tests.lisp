(defpackage "SB-POSIX-TESTS"
  (:use "COMMON-LISP" "SB-RT"))

(in-package "SB-POSIX-TESTS")

(defvar *test-directory*
  (ensure-directories-exist
   (merge-pathnames (make-pathname :directory '(:relative "test-lab"))
		    (make-pathname :directory
				   (pathname-directory *load-truename*)))))

(defvar *current-directory* *default-pathname-defaults*)

(defvar *this-file* *load-truename*)

(deftest chdir.1
  (sb-posix:chdir *test-directory*)
  0)

(deftest chdir.2
  (sb-posix:chdir (namestring *test-directory*))
  0)

(deftest chdir.3
  (sb-posix:chdir "/")
  0)

(deftest chdir.4
  (sb-posix:chdir #p"/")
  0)

(deftest chdir.5
  (sb-posix:chdir *current-directory*)
  0)

(deftest chdir.error.1
  (let ((dne (make-pathname :directory '(:relative "chdir.does-not-exist"))))
    (handler-case
	(sb-posix:chdir (merge-pathnames dne *test-directory*))
      (sb-posix:syscall-error (c)
	(sb-posix:syscall-errno c))))
  #.sb-posix::enoent)

(deftest chdir.error.2
  (handler-case
      (sb-posix:chdir *this-file*)
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::enotdir)

(deftest mkdir.1
  (let ((dne (make-pathname :directory '(:relative "mkdir.does-not-exist.1"))))
    (unwind-protect
	 (sb-posix:mkdir (merge-pathnames dne *test-directory*) 0)
      ;; FIXME: no delete-directory in CL, but using our own operators
      ;; is probably not ideal
      (ignore-errors (sb-posix:rmdir (merge-pathnames dne *test-directory*)))))
  0)

(deftest mkdir.2
  (let ((dne (make-pathname :directory '(:relative "mkdir.does-not-exist.2"))))
    (unwind-protect
	 (sb-posix:mkdir (namestring (merge-pathnames dne *test-directory*)) 0)
      (ignore-errors (sb-posix:rmdir (merge-pathnames dne *test-directory*)))))
  0)

(deftest mkdir.error.1
  (handler-case
      (sb-posix:mkdir *test-directory* 0)
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::eexist)

(deftest mkdir.error.2
  (handler-case
      (sb-posix:mkdir "/" 0)
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::eexist)

(deftest mkdir.error.3
  (handler-case
      (sb-posix:mkdir "/almost-certainly-does-not-exist" 0)
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::eacces)

(deftest rmdir.1
  (let ((dne (make-pathname :directory '(:relative "rmdir.does-not-exist.1"))))
    (ensure-directories-exist (merge-pathnames dne *test-directory*))
    (sb-posix:rmdir (merge-pathnames dne *test-directory*)))
  0)

(deftest rmdir.2
  (let ((dne (make-pathname :directory '(:relative "rmdir.does-not-exist.2"))))
    (ensure-directories-exist (merge-pathnames dne *test-directory*))
    (sb-posix:rmdir (namestring (merge-pathnames dne *test-directory*))))
  0)

(deftest rmdir.error.1
  (let ((dne (make-pathname :directory '(:relative "rmdir.dne.error.1"))))
    (handler-case 
	(sb-posix:rmdir (merge-pathnames dne *test-directory*))
      (sb-posix:syscall-error (c)
	(sb-posix:syscall-errno c))))
  #.sb-posix::enoent)

(deftest rmdir.error.2
  (handler-case
      (sb-posix:rmdir *this-file*)
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::enotdir)

(deftest rmdir.error.3
  (handler-case
      (sb-posix:rmdir "/")
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::ebusy)

(deftest rmdir.error.4
  (let* ((dir (ensure-directories-exist
	       (merge-pathnames
		(make-pathname :directory '(:relative "rmdir.error.4"))
		*test-directory*)))
	 (file (make-pathname :name "foo" :defaults dir)))
    (with-open-file (s file :direction :output)
      (write "" :stream s))
    (handler-case
	(sb-posix:rmdir dir)
      (sb-posix:syscall-error (c)
	(delete-file file)
	(sb-posix:rmdir dir)
	(sb-posix:syscall-errno c))))
  #.sb-posix::enotempty)

(deftest rmdir.error.5
  (let* ((dir (merge-pathnames
	       (make-pathname :directory '(:relative "rmdir.error.5"))
	       *test-directory*))
	 (dir2 (merge-pathnames
		(make-pathname :directory '(:relative "unremovable"))
		dir)))
    (sb-posix:mkdir dir #xffffffff)
    (sb-posix:mkdir dir2 #xffffffff)
    (sb-posix:chmod dir 0)
    (handler-case
	(sb-posix:rmdir dir2)
      (sb-posix:syscall-error (c)
	(sb-posix:chmod dir (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))
	(sb-posix:rmdir dir2)
	(sb-posix:rmdir dir)
	(sb-posix:syscall-errno c))))
  #.sb-posix::eacces)

(deftest stat.1
  (let* ((stat (sb-posix:stat *test-directory*))
	 (mode (sb-posix::stat-mode stat)))
    ;; FIXME: Ugly ::s everywhere
    (logand mode (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec)))
  #.(logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))

(deftest stat.2
  (let* ((stat (sb-posix:stat "/root"))
	 (mode (sb-posix::stat-mode stat)))
    (logand mode sb-posix::s-iwoth))
  0)
    
(deftest stat.3
  (let* ((now (get-universal-time))
	 ;; FIXME: (encode-universal-time 00 00 00 01 01 1970)
	 (unix-now (- now 2208988800))
	 (stat (sb-posix:stat *test-directory*))
	 (atime (sb-posix::stat-atime stat)))
    ;; FIXME: breaks if mounted noatime :-(
    (< (- atime unix-now) 10))
  t)

;;; FIXME: add tests for carrying a stat structure around in the
;;; optional argument to SB-POSIX:STAT

(deftest stat.error.1
  (handler-case (sb-posix:stat "")
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::enoent)

(deftest stat.error.2
  (let* ((dir (merge-pathnames
	       (make-pathname :directory '(:relative "stat.error.2"))
	       *test-directory*))
	 (file (merge-pathnames
		(make-pathname :name "unstatable")
		dir)))
    (sb-posix:mkdir dir #xffffffff)
    (with-open-file (s file :direction :output)
      (write "" :stream s))
    (sb-posix:chmod dir 0)
    (handler-case
	(sb-posix:stat file)
      (sb-posix:syscall-error (c)
	(sb-posix:chmod dir (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))
	(sb-posix:unlink file)
	(sb-posix:rmdir dir)
	(sb-posix:syscall-errno c))))
  #.sb-posix::eacces)

;;; stat-mode tests
(defmacro with-stat-mode ((mode pathname) &body body)
  (let ((stat (gensym)))
    `(let* ((,stat (sb-posix:stat ,pathname))
            (,mode (sb-posix::stat-mode ,stat)))
       ,@body)))

(defmacro with-lstat-mode ((mode pathname) &body body)
  (let ((stat (gensym)))
    `(let* ((,stat (sb-posix:lstat ,pathname))
            (,mode (sb-posix::stat-mode ,stat)))
       ,@body)))

(deftest stat-mode.1
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-isreg mode))
  0)

(deftest stat-mode.2
  (with-stat-mode (mode *test-directory*)
    (zerop (sb-posix:s-isdir mode)))
  nil)

(deftest stat-mode.3
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-ischr mode))
  0)

(deftest stat-mode.4
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-isblk mode))
  0)

(deftest stat-mode.5
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-isfifo mode))
  0)

(deftest stat-mode.6
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-issock mode))
  0)

(deftest stat-mode.7
  (let ((link-pathname (make-pathname :name "stat-mode.7"
                                      :defaults *test-directory*)))
    (unwind-protect
         (progn
           (sb-posix:symlink *test-directory* link-pathname)
           (with-lstat-mode (mode link-pathname)
             (zerop (sb-posix:s-islnk mode))))
      (ignore-errors (sb-posix:unlink link-pathname))))
  nil)

(deftest stat-mode.8
  (let ((pathname (make-pathname :name "stat-mode.8"
                                 :defaults *test-directory*)))
    (unwind-protect
         (progn
           (with-open-file (out pathname :direction :output)
             (write-line "test" out))
           (with-stat-mode (mode pathname)
             (zerop (sb-posix:s-isreg mode))))
      (ignore-errors (delete-file pathname))))
  nil)

;;; see comment in filename's designator definition, in macros.lisp
(deftest filename-designator.1
  (let ((file (format nil "~A/[foo].txt" (namestring *test-directory*))))
    ;; creat() with a string as argument
    (sb-posix:creat file 0)
    ;; if this test fails, it will probably be with
    ;; "System call error 2 (No such file or directory)"
    (let ((*default-pathname-defaults* *test-directory*))
      (sb-posix:unlink (car (directory "*.txt")))))
  0)
			 
