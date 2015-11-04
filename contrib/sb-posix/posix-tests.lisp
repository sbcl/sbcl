(defpackage "SB-POSIX-TESTS"
  (:use "COMMON-LISP" "SB-RT"))

(in-package "SB-POSIX-TESTS")

(defvar *test-directory*
  (ensure-directories-exist
   (merge-pathnames (make-pathname :directory '(:relative "test-output"))
                    (make-pathname :directory
                                   (pathname-directory *load-truename*)))))

(defvar *current-directory* *default-pathname-defaults*)

(defvar *this-file* *load-truename*)

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +mode-rwx-all+
    (logior sb-posix::s-irusr sb-posix::s-iwusr sb-posix::s-ixusr
            #-win32
            (logior
             sb-posix::s-irgrp sb-posix::s-iwgrp sb-posix::s-ixgrp
             sb-posix::s-iroth sb-posix::s-iwoth sb-posix::s-ixoth))))

(defmacro define-eacces-test (name form &rest values)
  #-win32
  `(deftest ,name
    (block ,name
      (when (= (sb-posix:geteuid) 0)
        (return-from ,name (values ,@values)))
      ,form)
    ,@values))

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

(deftest chdir.6
  (sb-posix:chdir "/../")
  0)

(deftest chdir.7
  (sb-posix:chdir #p"/../")
  0)

(deftest chdir.8
  (sb-posix:chdir (make-pathname :directory '(:absolute :up)))
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
  #-win32
  #.sb-posix:enotdir
  #+win32
  #.sb-posix:einval)

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
      (sb-posix:mkdir #-win32 "/" #+win32 "C:/" 0)
    (sb-posix:syscall-error (c)
      ;; The results aren't the most consistent ones across platforms. Darwin
      ;; likes EISDIR, Windows either EACCESS or EEXIST, others EEXIST.
      ;; ...let's just accept them all.
      (when (member (sb-posix:syscall-errno c)
                    (list #.sb-posix:eisdir
                          #.sb-posix:eacces
                          #.sb-posix::eexist)
                    :test #'eql)
        :ok)))
  :ok)

(define-eacces-test mkdir.error.3
  (let* ((dir (merge-pathnames
               (make-pathname :directory '(:relative "mkdir.error.3"))
               *test-directory*))
         (dir2 (merge-pathnames
                (make-pathname :directory '(:relative "does-not-exist"))
                dir)))
    (sb-posix:mkdir dir 0)
    (handler-case
        (sb-posix:mkdir dir2 0)
      (sb-posix:syscall-error (c)
        (sb-posix:rmdir dir)
        (sb-posix:syscall-errno c))
      (:no-error (result)
        (sb-posix:rmdir dir2)
        (sb-posix:rmdir dir)
        result)))
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
  #-win32
  #.sb-posix::enotdir
  #+win32
  #.sb-posix::einval)

(deftest rmdir.error.3
  (handler-case
      (sb-posix:rmdir #-win32 "/" #+win32 (sb-ext:posix-getenv "windir"))
    (sb-posix:syscall-error (c)
      (typep
       (sb-posix:syscall-errno c)
       '(member
         #+(or darwin openbsd)
         #.sb-posix:eisdir
         #+win32
         #.sb-posix::eacces
         #+win32
         #.sb-posix::enotempty
         #+sunos
         #.sb-posix::einval
         #-(or darwin openbsd win32 sunos)
         #.sb-posix::ebusy)))) t)

(deftest rmdir.error.4
  (let* ((dir (ensure-directories-exist
               (merge-pathnames
                (make-pathname :directory '(:relative "rmdir.error.4"))
                *test-directory*)))
         (file (make-pathname :name "foo" :defaults dir)))
    (with-open-file (s file :direction :output :if-exists nil)
      (write "" :stream s))
    (handler-case
        (sb-posix:rmdir dir)
      (sb-posix:syscall-error (c)
        (delete-file file)
        (sb-posix:rmdir dir)
        (let ((errno (sb-posix:syscall-errno c)))
          ;; documented by POSIX
          (or (= errno sb-posix::eexist) (= errno sb-posix::enotempty))))))
  t)

(define-eacces-test rmdir.error.5
  (let* ((dir (merge-pathnames
               (make-pathname :directory '(:relative "rmdir.error.5"))
               *test-directory*))
         (dir2 (merge-pathnames
                (make-pathname :directory '(:relative "unremovable"))
                dir)))
    (sb-posix:mkdir dir +mode-rwx-all+)
    (sb-posix:mkdir dir2 +mode-rwx-all+)
    (sb-posix:chmod dir 0)
    (handler-case
        (sb-posix:rmdir dir2)
      (sb-posix:syscall-error (c)
        (sb-posix:chmod dir (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))
        (sb-posix:rmdir dir2)
        (sb-posix:rmdir dir)
        (sb-posix:syscall-errno c))
      (:no-error (result)
        (sb-posix:chmod dir (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))
        (sb-posix:rmdir dir)
        result)))
  #.sb-posix::eacces)

(deftest stat.1
  (let* ((stat (sb-posix:stat *test-directory*))
         (mode (sb-posix::stat-mode stat)))
    ;; FIXME: Ugly ::s everywhere
    (logand mode (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec)))
  #.(logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))

#-win32
(deftest stat.2
  (let* ((stat (sb-posix:stat "/"))
         (mode (sb-posix::stat-mode stat)))
    ;; it's logically possible for / to be writeable by others... but
    ;; if it is, either someone is playing with strange security
    ;; modules or they want to know about it anyway.
    (logand mode sb-posix::s-iwoth))
  0)

(deftest stat.3
  (let* ((now (get-universal-time))
         ;; FIXME: (encode-universal-time 00 00 00 01 01 1970)
         (unix-now (- now 2208988800))
         (stat (sb-posix:stat *test-directory*))
         (atime (sb-posix::stat-atime stat)))
    ;; FIXME: breaks if mounted noatime :-(
    #+nil (< (- atime unix-now) 10)
    (< (- atime unix-now) 10))
  t)

#-win32
(deftest stat.4
  (let* ((stat (sb-posix:stat (make-pathname :directory '(:absolute :up))))
         (mode (sb-posix::stat-mode stat)))
    ;; it's logically possible for / to be writeable by others... but
    ;; if it is, either someone is playing with strange security
    ;; modules or they want to know about it anyway.
    (logand mode sb-posix::s-iwoth))
  0)

;; Test that stat can take a second argument.
#-win32
(deftest stat.5
    (let* ((stat-1 (sb-posix:stat "/"))
           (inode-1 (sb-posix:stat-ino stat-1))
           (stat-2 (sb-posix:stat "/bin/sh"
                                   stat-1))
           (inode-2 (sb-posix:stat-ino stat-2)))
      (values
       (eq stat-1 stat-2)
       (/= inode-1 inode-2)))
  t
  t)

#+win32
(deftest stat.5
    (let ((f (namestring (merge-pathnames "some.file" *test-directory*))))
      (close (open f :if-exists :append :if-does-not-exist :create))
      (let* ((stat-1 (sb-posix:stat "/"))
             (mode-1 (sb-posix:stat-mode stat-1))
             (stat-2 (sb-posix:stat f stat-1))
             (mode-2 (sb-posix:stat-mode stat-2)))
        (values
         (eq stat-1 stat-2)
         (/= mode-1 mode-2))))
  t
  t)

;;; FIXME: add tests for carrying a stat structure around in the
;;; optional argument to SB-POSIX:STAT

(deftest stat.error.1
  (handler-case (sb-posix:stat "")
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #.sb-posix::enoent)

(define-eacces-test stat.error.2
  (let* ((dir (merge-pathnames
               (make-pathname :directory '(:relative "stat.error.2"))
               *test-directory*))
         (file (merge-pathnames
                (make-pathname :name "unstatable")
                dir)))
    (sb-posix:mkdir dir +mode-rwx-all+)
    (with-open-file (s file :direction :output)
      (write "" :stream s))
    (sb-posix:chmod dir 0)
    (handler-case
        (sb-posix:stat file)
      (sb-posix:syscall-error (c)
        (sb-posix:chmod dir (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))
        (sb-posix:unlink file)
        (sb-posix:rmdir dir)
        (sb-posix:syscall-errno c))
      (:no-error (result)
        (sb-posix:chmod dir (logior sb-posix::s-iread sb-posix::s-iwrite sb-posix::s-iexec))
        (sb-posix:unlink file)
        (sb-posix:rmdir dir)
        result)))
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
  nil)

(deftest stat-mode.2
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-isdir mode))
  t)

(deftest stat-mode.3
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-ischr mode))
  nil)

(deftest stat-mode.4
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-isblk mode))
  nil)

(deftest stat-mode.5
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-isfifo mode))
  nil)

#-win32
(deftest stat-mode.6
  (with-stat-mode (mode *test-directory*)
    (sb-posix:s-issock mode))
  nil)

#-win32
(deftest stat-mode.7
  (let ((link-pathname (make-pathname :name "stat-mode.7"
                                      :defaults *test-directory*)))
    (unwind-protect
         (progn
           (sb-posix:symlink *test-directory* link-pathname)
           (with-lstat-mode (mode link-pathname)
             (sb-posix:s-islnk mode)))
      (ignore-errors (sb-posix:unlink link-pathname))))
  t)

(deftest stat-mode.8
  (let ((pathname (make-pathname :name "stat-mode.8"
                                 :defaults *test-directory*)))
    (unwind-protect
         (progn
           (with-open-file (out pathname :direction :output)
             (write-line "test" out))
           (with-stat-mode (mode pathname)
             (sb-posix:s-isreg mode)))
      (ignore-errors (delete-file pathname))))
  t)

;;; see comment in filename's designator definition, in macros.lisp
(deftest filename-designator.1
  (let ((file (format nil "~A/[foo].txt" (namestring *test-directory*))))
    ;; creat() with a string as argument
    (let ((fd (sb-posix:creat file sb-posix:s-iwrite)))
      #+win32
      (sb-posix:close fd))
    ;; if this test fails, it will probably be with
    ;; "System call error 2 (No such file or directory)"
    (let ((*default-pathname-defaults* *test-directory*))
      (sb-posix:unlink (car (directory "*.txt")))))
  0)

(deftest open.1
    (let ((name (merge-pathnames "open-test.txt" *test-directory*)))
      (unwind-protect
           (progn
             (sb-posix:close
              (sb-posix:creat name (logior sb-posix:s-iwrite sb-posix:s-iread)))
             (let ((fd (sb-posix:open name sb-posix::o-rdonly)))
               (ignore-errors (sb-posix:close fd))
               (< fd 0)))
        (ignore-errors (sb-posix:unlink name))))
  nil)

#-hpux ; fix: cant handle c-vargs
(deftest open.error.1
  (handler-case (sb-posix:open *test-directory* sb-posix::o-wronly)
    (sb-posix:syscall-error (c)
      (sb-posix:syscall-errno c)))
  #-win32
  #.sb-posix::eisdir
  #+win32
  #.sb-posix:eacces)

#-(or (and (or x86-64 arm64 alpha) (or linux sunos)) win32)
(deftest fcntl.1
  (let ((fd (sb-posix:open "/dev/null" sb-posix::o-nonblock)))
    (= (sb-posix:fcntl fd sb-posix::f-getfl) sb-posix::o-nonblock))
  t)
;; On AMD64/Linux O_LARGEFILE is always set, even though the whole
;; flag makes no sense.
#+(and (or x86-64 arm64 alpha) (or linux sunos))
(deftest fcntl.1
  (let ((fd (sb-posix:open "/dev/null" sb-posix::o-nonblock)))
    (/= 0 (logand (sb-posix:fcntl fd sb-posix::f-getfl)
                  sb-posix::o-nonblock)))
  t)

#-(or hpux win32 netbsd) ; fix: cant handle c-vargs
(deftest fcntl.flock.1
    (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let ((flock (make-instance 'sb-posix:flock
                      :type sb-posix:f-wrlck
                      :whence sb-posix:seek-set
                      :start 0 :len 10))
            (pathname "fcntl.flock.1")
            kid-status)
        (catch 'test
          (with-open-file (f pathname :direction :output)
            (write-line "1234567890" f)
            (assert (zerop (sb-posix:fcntl f sb-posix:f-setlk flock)))
            (let ((pid (sb-posix:fork)))
              (if (zerop pid)
                  (progn
                    (multiple-value-bind (nope error)
                        (ignore-errors (sb-posix:fcntl f sb-posix:f-setlk flock))
                      (sb-ext:exit
                       :code
                       (cond ((not (null nope)) 1)
                             ((= (sb-posix:syscall-errno error) sb-posix:eagain)
                              42)
                             (t 86))
                       :abort t #| don't delete the file |#)))
                  (progn
                    (setf kid-status
                          (sb-posix:wexitstatus
                           (nth-value
                            1 (sb-posix:waitpid pid 0))))
                    (throw 'test nil))))))
        kid-status))
  42)


#-(or win32 netbsd)
(deftest fcntl.flock.2
    (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let ((flock (make-instance 'sb-posix:flock
                      :type sb-posix:f-wrlck
                      :whence sb-posix:seek-set
                      :start 0 :len 10))
            (pathname "fcntl.flock.2")
            kid-status)
        (catch 'test
          (with-open-file (f pathname :direction :output)
            (write-line "1234567890" f)
            (assert (zerop (sb-posix:fcntl f sb-posix:f-setlk flock)))
            (let ((ppid (sb-posix:getpid))
                  (pid (sb-posix:fork)))
              (if (zerop pid)
                  (let ((r (sb-posix:fcntl f sb-posix:f-getlk flock)))
                    (sb-ext:exit
                     :code
                     (cond ((not (zerop r)) 1)
                           ((= (sb-posix:flock-pid flock) ppid) 42)
                           (t 86))
                     :abort t #| don't delete the file |#))
                  (progn
                    (setf kid-status
                          (sb-posix:wexitstatus
                           (nth-value
                            1 (sb-posix:waitpid pid 0))))
                    (throw 'test nil))))))
        kid-status))
  42)

(deftest read.1
    (progn
      (with-open-file (ouf (merge-pathnames "read-test.txt" *test-directory*)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (write-string "foo" ouf))
      (let ((fd (sb-posix:open (merge-pathnames "read-test.txt" *test-directory*) sb-posix:o-rdonly)))
        (unwind-protect
             (let ((buf (make-array 10 :element-type '(unsigned-byte 8))))
               (values
                (sb-posix:read fd (sb-sys:vector-sap buf) 10)
                (code-char (aref buf 0))
                (code-char (aref buf 1))
                (code-char (aref buf 2))))
          (sb-posix:close fd))))
  3 #\f #\o #\o)

(deftest opendir.1
  (let ((dir (sb-posix:opendir "/")))
    (unwind-protect (sb-alien:null-alien dir)
      (unless (sb-alien:null-alien dir)
        (sb-posix:closedir dir))))
  nil)

(deftest readdir.1
  (let ((dir (sb-posix:opendir "/")))
    (unwind-protect
       (block dir-loop
         (loop for dirent = (sb-posix:readdir dir)
               until (sb-alien:null-alien dirent)
               when (not (stringp (sb-posix:dirent-name dirent)))
                 do (return-from dir-loop nil)
               finally (return t)))
      (sb-posix:closedir dir)))
  t)

#-darwin
(deftest readdir/dirent-name
    (let ((dir (sb-posix:opendir *current-directory*)))
      (unwind-protect
           (equal (sort (loop for entry = (sb-posix:readdir dir)
                           until (sb-alien:null-alien entry)
                           collect (sb-posix:dirent-name entry))
                        #'string<)
                  (sort (append '("." "..")
                                (mapcar (lambda (p)
                                          (let ((string (enough-namestring p *current-directory*)))
                                            (if (pathname-name p)
                                                string
                                                (subseq string 0 (1- (length string))))))
                                        (directory (make-pathname
                                                    :name :wild
                                                    :type :wild
                                                    :defaults *current-directory*))))
                        #'string<))
        (sb-posix:closedir dir)))
  t)


(deftest write.1
    (progn
      (let ((fd (sb-posix:open (merge-pathnames "write-test.txt" *test-directory*)
                               (logior sb-posix:o-creat sb-posix:o-wronly)
                               (logior sb-posix:s-irusr sb-posix:s-iwusr)))
            (retval nil))
        (unwind-protect
             (let ((buf (coerce "foo" 'simple-base-string)))
               (setf retval (sb-posix:write fd (sb-sys:vector-sap buf) 3)))
          (sb-posix:close fd))

        (with-open-file (inf (merge-pathnames "write-test.txt" *test-directory*)
                             :direction :input)
          (values retval (read-line inf)))))
  3 "foo")

#-(or android win32)
(deftest pwent.1
  ;; make sure that we found something
  (not (sb-posix:getpwuid 0))
  nil)

#-(or android win32)
(deftest pwent.2
  ;; make sure that we found something
  (not (sb-posix:getpwnam "root"))
  nil)

#-(or android win32)
(deftest pwent.non-existing
    ;; make sure that we get something sensible, not an error
    (handler-case (progn (sb-posix:getpwnam "almost-certainly-does-not-exist")
                         nil)
      (t (cond) t))
  nil)

#-(or android win32)
(deftest grent.1
  ;; make sure that we found something
  (not (sb-posix:getgrgid 0))
  nil)

#-(or android win32)
(deftest grent.2
  ;; make sure that we found something
  (not (sb-posix:getgrnam "sys"))
  nil)

#-(or android win32)
(deftest grent.non-existing
    ;; make sure that we get something sensible, not an error
    (handler-case (progn (sb-posix:getgrnam "almost-certainly-does-not-exist")
                         nil)
      (t (cond) t))
  nil)

#+nil
;; Requires root or special group + plus a sensible thing on the port
(deftest cfget/setispeed.1
    (with-open-file (s "/dev/ttyS0")
      (let* ((termios (sb-posix:tcgetattr s))
             (old (sb-posix:cfgetispeed termios))
             (new (if (= old sb-posix:b2400)
                      sb-posix:b9600
                      sb-posix:b2400)))
        (sb-posix:cfsetispeed new termios)
        (sb-posix:tcsetattr 0 sb-posix:tcsadrain termios)
        (setf termios (sb-posix:tcgetattr s))
        (= new (sb-posix:cfgetispeed termios))))
  t)

#+nil
;; Requires root or special group + a sensible thing on the port
(deftest cfget/setospeed.1
    (with-open-file (s "/dev/ttyS0" :direction :output :if-exists :append)
      (let* ((termios (sb-posix:tcgetattr s))
             (old (sb-posix:cfgetospeed termios))
             (new (if (= old sb-posix:b2400)
                      sb-posix:b9600
                      sb-posix:b2400)))
        (sb-posix:cfsetospeed new termios)
        (sb-posix:tcsetattr 0 sb-posix:tcsadrain termios)
        (setf termios (sb-posix:tcgetattr 0))
        (= new (sb-posix:cfgetospeed termios))))
  t)


#-win32
(deftest time.1
    (plusp (sb-posix:time))
  t)

#-(or win32)
(deftest utimes.1
    (let ((file (merge-pathnames #p"utimes.1" *test-directory*))
          (atime (random (1- (expt 2 31))))
          (mtime (random (1- (expt 2 31)))))
      (with-open-file (stream file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (princ "Hello, utimes" stream))
      (sb-posix:utime file atime mtime)
      (let* ((stat (sb-posix:stat file)))
        (delete-file file)
        (list (= (sb-posix:stat-atime stat) atime)
              (= (sb-posix:stat-mtime stat) mtime))))
  (t t))

;; readlink tests.
#-win32
(progn
  (deftest readlink.1
      (let ((link-pathname (make-pathname :name "readlink.1"
                                          :defaults *test-directory*)))
        (sb-posix:symlink "/" link-pathname)
        (unwind-protect
             (sb-posix:readlink link-pathname)
          (ignore-errors (sb-posix:unlink link-pathname))))
    "/")

  ;; Same thing, but with a very long link target (which doesn't have
  ;; to exist).  This tests the array adjustment in the wrapper,
  ;; provided that the target's length is long enough.
  #-hpux ; arg2 to readlink is 80, and arg0 is larger than that
  (deftest readlink.2
      (let ((target-pathname (make-pathname
                              :name (make-string 255 :initial-element #\a)
                              :directory '(:absolute)))
            (link-pathname (make-pathname :name "readlink.2"
                                          :defaults *test-directory*)))
        (sb-posix:symlink target-pathname link-pathname)
        (unwind-protect
             (sb-posix:readlink link-pathname)
          (ignore-errors (sb-posix:unlink link-pathname))))
    #.(concatenate 'string "/" (make-string 255 :initial-element #\a)))

  ;; The error tests are in the order of exposition from SUSv3.
  (deftest readlink.error.1
      (let* ((subdir-pathname (merge-pathnames
                               (make-pathname
                                :directory '(:relative "readlink.error.1"))
                               *test-directory*))
             (link-pathname (make-pathname :name "readlink.error.1"
                                           :defaults subdir-pathname)))
        (sb-posix:mkdir subdir-pathname #o777)
        (sb-posix:symlink "/" link-pathname)
        (sb-posix:chmod subdir-pathname 0)
        (unwind-protect
             (handler-case (sb-posix:readlink link-pathname)
               (sb-posix:syscall-error (c)
                 (sb-posix:syscall-errno c)))
          (ignore-errors
            (sb-posix:chmod subdir-pathname #o777)
            (sb-posix:unlink link-pathname)
            (sb-posix:rmdir subdir-pathname))))
    #.sb-posix:eacces)
  (deftest readlink.error.2
      (let* ((non-link-pathname (make-pathname :name "readlink.error.2"
                                               :defaults *test-directory*))
             (fd (sb-posix:open non-link-pathname sb-posix::o-creat)))
        (unwind-protect
             (handler-case (sb-posix:readlink non-link-pathname)
               (sb-posix:syscall-error (c)
                 (sb-posix:syscall-errno c)))
          (ignore-errors
            (sb-posix:close fd)
            (sb-posix:unlink non-link-pathname))))
    #.sb-posix:einval)
  ;; Skipping EIO, ELOOP
  (deftest readlink.error.3
      (let* ((link-pathname (make-pathname :name "readlink.error.3"
                                           :defaults *test-directory*))
             (bogus-pathname (merge-pathnames
                              (make-pathname
                               :name "bogus"
                               :directory '(:relative "readlink.error.3"))
                               *test-directory*)))
        (sb-posix:symlink link-pathname link-pathname)
        (unwind-protect
             (handler-case (sb-posix:readlink bogus-pathname)
               (sb-posix:syscall-error (c)
                 (sb-posix:syscall-errno c)))
          (ignore-errors (sb-posix:unlink link-pathname))))
    #.sb-posix:eloop)
  ;; Note: PATH_MAX and NAME_MAX need not be defined, and may vary, so
  ;; failure of this test is not too meaningful.
  (deftest readlink.error.4
      (let ((pathname
             (make-pathname :name (make-string 257 ;NAME_MAX plus some, maybe
                                               :initial-element #\a))))
        (handler-case (sb-posix:readlink pathname)
          (sb-posix:syscall-error (c)
            (sb-posix:syscall-errno c))))
    #.sb-posix:enametoolong)
  (deftest readlink.error.5
      (let ((string (format nil "~v{/A~}" 2049 ;PATH_MAX/2 plus some, maybe
                                          '(x))))
        (handler-case (sb-posix:readlink string)
          (sb-posix:syscall-error (c)
            (sb-posix:syscall-errno c))))
    #.sb-posix:enametoolong)
    (deftest readlink.error.6
      (let ((no-such-pathname (make-pathname :name "readlink.error.6"
                                             :defaults *test-directory*)))
        (handler-case (sb-posix:readlink no-such-pathname)
          (sb-posix:syscall-error (c)
            (sb-posix:syscall-errno c))))
    #.sb-posix:enoent)
  (deftest readlink.error.7
      (let* ((non-link-pathname (make-pathname :name "readlink.error.7"
                                               :defaults *test-directory*))
             (impossible-pathname (merge-pathnames
                                   (make-pathname
                                    :directory
                                    '(:relative "readlink.error.7")
                                    :name "readlink.error.7")
                                   *test-directory*))
             (fd (sb-posix:open non-link-pathname sb-posix::o-creat)))
        (unwind-protect
             (handler-case (sb-posix:readlink impossible-pathname)
               (sb-posix:syscall-error (c)
                 (sb-posix:syscall-errno c)))
          (ignore-errors
            (sb-posix:close fd)
            (sb-posix:unlink non-link-pathname))))
    #.sb-posix:enotdir)
  )

(deftest getcwd.1
    ;; FIXME: something saner, please
    (equal (sb-unix::posix-getcwd) (sb-posix:getcwd))
  t)

#-win32
(deftest mkstemp.1
    (multiple-value-bind (fd temp)
        (sb-posix:mkstemp (make-pathname
                           :name "mkstemp-1"
                           :type "XXXXXX"
                           :defaults *test-directory*))
      (let ((pathname (sb-ext:parse-native-namestring temp)))
        (unwind-protect
             (values (integerp fd) (pathname-name pathname))
          (delete-file temp))))
  t "mkstemp-1")

;#-(or win32 sunos hpux)
;;;; mkdtemp is unimplemented on at least Solaris 10
#-(or win32 hpux sunos)
;;; But it is implemented on OpenSolaris 2008.11
(deftest mkdtemp.1
    (let ((pathname
           (sb-ext:parse-native-namestring
            (sb-posix:mkdtemp (make-pathname
                               :name "mkdtemp-1"
                               :type "XXXXXX"
                               :defaults *test-directory*))
            nil
            *default-pathname-defaults*
            :as-directory t)))
      (unwind-protect
           (values (let* ((xxx (car (last (pathname-directory pathname))))
                          (p (position #\. xxx)))
                     (and p (subseq xxx 0 p)))
                   (pathname-name pathname)
                   (pathname-type pathname))
        (sb-posix:rmdir pathname)))
  "mkdtemp-1" nil nil)

#-win32
(deftest mktemp.1
    (let ((pathname (sb-ext:parse-native-namestring
                     (sb-posix:mktemp #p"mktemp.XXXXXX"))))
      (values (equal "mktemp" (pathname-name pathname))
              (not (equal "XXXXXX" (pathname-type pathname)))))
  t t)

#-win32
(deftest mkstemp.null-terminate
    (let* ((default (make-pathname :directory '(:absolute "tmp")))
           (filename (namestring (make-pathname :name "mkstemp-1"
                                                :type "XXXXXX"
                                                :defaults default)))
           ;; The magic 64 is the filename length that happens to
           ;; trigger the no null termination bug at least on my
           ;; machine on a certain build.
           (n (- 64 (length (sb-ext:string-to-octets filename)))))
      (multiple-value-bind (fd temp)
          (sb-posix:mkstemp (make-pathname
                             :name "mkstemp-1"
                             :type (format nil "~AXXXXXX"
                                           (make-string n :initial-element #\x))
                             :defaults default))
          (unwind-protect
               (values (integerp fd) (subseq temp 0 (position #\. temp)))
            (delete-file temp))))
  t "/tmp/mkstemp-1")

(deftest envstuff
    (let ((name1 "ASLIFJLSDKFJKAHGSDKLJH")
          (name2 "KJHFKLJDSHIUYHBSDNFCBH"))
      (values (sb-posix:getenv name1)
              (sb-posix:getenv name1)
              (progn
                (sb-posix:putenv (concatenate 'string name1 "=name1,test1"))
                (sb-ext:gc :full t)
                (sb-posix:getenv name1))
              (progn
                (sb-posix:setenv name1 "name1,test2" 0)
                (sb-ext:gc :full t)
                (sb-posix:getenv name1))
              (progn
                (sb-posix:setenv name2 "name2,test1" 0)
                (sb-ext:gc :full t)
                (sb-posix:getenv name2))
              (progn
                (sb-posix:setenv name2 "name2,test2" 1)
                (sb-ext:gc :full t)
                (sb-posix:getenv name2))))
  nil
  nil
  "name1,test1"
  "name1,test1"
  "name2,test1"
  "name2,test2")
