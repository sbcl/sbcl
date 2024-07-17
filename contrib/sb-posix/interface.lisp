(cl:in-package :sb-posix)

(defmacro define-protocol-class
    (name alien-type superclasses slots &rest options)
  (let ((to-protocol (intern (format nil "ALIEN-TO-~A" name)))
        (to-alien (intern (format nil "~A-TO-ALIEN" name))))
    `(progn
      (export ',name :sb-posix)
      (defclass ,name ,superclasses
         ;; KLUDGE: Splice out some slot options (they're
         ;; for the conversion functions, not for DEFCLASS).
        ,(loop for slotd in slots
               collect
               (let ((slotd (copy-list slotd)))
                 (dolist (keyword '(:array-length :from-alien) slotd)
                   (remf (cdr slotd) keyword))))
        ,@options)
      ;; TODO (maybe): there's no reason to define to-alien routines
      ;; struct stat, passwd, or group: OS interfaces only ever write
      ;; into them, never read from them.
      (declaim (inline ,to-alien ,to-protocol))
      (declaim (inline ,to-protocol ,to-alien))
      (defun ,to-protocol (alien &optional instance)
        (declare (type (sb-alien:alien (* ,alien-type)) alien)
                 (type (or null ,name) instance))
        (unless instance
          (setf instance (make-instance ',name)))
        ,@(loop for slotd in slots
                ;; FIXME: slotds in source are more complicated in general
                ;;
                ;; FIXME: baroque construction of intricate fragility
                for array-length = (getf (cdr slotd) :array-length)
                for from-alien = (getf (cdr slotd) :from-alien)
                if array-length
                  collect `(progn
                             (let ((array (make-array ,array-length)))
                               (setf (slot-value instance ',(car slotd))
                                     array)
                               (dotimes (i ,array-length)
                                 (setf (aref array i)
                                       (sb-alien:deref
                                        (sb-alien:slot alien ',(car slotd))
                                        i)))))
                else
                  collect (if from-alien
                              ;; FROM-ALIEN is for any ad-hoc conversions that
                              ;; SB-ALIEN doesn't automatically handle, such as
                              ;; char** to list of strings.
                              `(setf (slot-value instance ',(car slotd))
                                     (,from-alien (sb-alien:slot alien ',(car slotd))))
                              `(setf (slot-value instance ',(car slotd))
                                     (sb-alien:slot alien ',(car slotd)))))
        instance)
      (defun ,to-alien (instance &optional alien)
        (declare (type (or null (sb-alien:alien (* ,alien-type))) alien)
                 (type ,name instance))
        (unless alien
          (setf alien (sb-alien:make-alien ,alien-type)))
        ,@(loop for slotd in slots
                for array-length = (getf (cdr slotd) :array-length)
                if array-length
                  collect `(progn
                             (let ((array (slot-value instance ',(car slotd))))
                               (dotimes (i ,array-length)
                                 (setf (sb-alien:deref
                                        (sb-alien:slot alien ',(car slotd))
                                        i)
                                       (aref array i)))))
                ;; N.B., nothing turns out to need a :TO-ALIEN
                ;; counterpart of :FROM-ALIEN so far.
                else
                  collect `(setf (sb-alien:slot alien ',(car slotd))
                                 (slot-value instance ',(car slotd)))))
      (find-class ',name))))

(define-condition sb-posix:syscall-error (error)
  ((errno :initarg :errno :reader sb-posix:syscall-errno)
   (name :initarg :name :initform nil :reader sb-posix:syscall-name))
  (:report (lambda (c s)
             (let ((errno (sb-posix:syscall-errno c))
                   (name (sb-posix:syscall-name c)))
               (if name
                   (format s "Error in ~S: ~A (~A)"
                           name
                           (sb-int:strerror errno)
                           errno)
                   (format s "Error in syscall: ~A (~A)"
                           (sb-int:strerror errno)
                           errno))))))

(declaim (ftype (function (&optional symbol) nil) syscall-error))
(defun syscall-error (&optional name)
  (error 'sb-posix:syscall-error
         :name name
         :errno (get-errno)))

(defun unsupported-error (lisp-name c-name)
  (error "~S is unsupported by SBCL on this platform due to lack of ~A()."
         lisp-name c-name))

(defun unsupported-warning (lisp-name c-name)
  (warn "~S is unsupported by SBCL on this platform due to lack of ~A()."
        lisp-name c-name))

(declaim (inline never-fails))
(defun never-fails (&rest args)
  (declare (ignore args))
  nil)

;;; Some systems may need C-level wrappers, which can live in the
;;; runtime (so that save-lisp-and-die can produce standalone
;;; executables).  See REAL-C-NAME in macros.lisp for the use of this
;;; variable.
(eval-when (:compile-toplevel :load-toplevel)
  (setf *c-functions-in-runtime*
        (append #+netbsd '("stat" "lstat" "fstat" "readdir" "opendir")
                #+(and (or arm64 riscv ppc ppc64) linux) '("stat" "lstat" "fstat"))))


;;; filesystem access
(defmacro define-call* (name &rest arguments)
  #-win32 `(define-call ,name ,@arguments)
  #+win32 `(define-call ,(if (consp name)
                             `(,(concatenate 'string "_" (car name))
                                ,@(cdr name))
                             (concatenate 'string "_" name))
               ,@arguments))

(define-call* "access" int minusp (pathname filename) (mode int))
(define-call* "chdir" int minusp (pathname filename))
(define-call* "chmod" int minusp (pathname filename) (mode mode-t))
(define-call* "close" int minusp (fd file-descriptor))
(define-call* "creat" int minusp (pathname filename) (mode mode-t))
(define-call* "dup" int minusp (oldfd file-descriptor))
(define-call* "dup2" int minusp (oldfd file-descriptor)
              (newfd file-descriptor))
(define-call* ("lseek" :options :largefile)
    off-t minusp (fd file-descriptor) (offset off-t)
    (whence int))
(define-call* "mkdir" int minusp (pathname filename) (mode mode-t))
(macrolet ((def (x)
               `(progn
                 (define-call-internally open-with-mode ,x int minusp
                   (pathname filename) (flags int) &optional (mode mode-t))
                 (define-call-internally open-without-mode ,x int minusp
                   (pathname filename) (flags int))
                 (define-entry-point ,x
                     (pathname flags &optional (mode nil mode-supplied))
                   (if mode-supplied
                       (open-with-mode pathname flags mode)
                       (open-without-mode pathname flags))))))
    (def #-win32 "open" #+win32 "_open"))
(define-call "rename" int minusp (oldpath filename) (newpath filename))
(define-call* "rmdir" int minusp (pathname filename))
(define-call* "unlink" int minusp (pathname filename))
(define-call #-netbsd "opendir" #+netbsd "_opendir"
    (* t) null-alien (pathname filename))
(define-call* "read" ssize-t minusp
    (fd file-descriptor) (buf (* t)) (count size-t))
(define-call* "write" ssize-t minusp
  (fd file-descriptor) (buf (* t)) (count size-t))

(declaim (inline null-alien-and-errno-plusp))
(defun null-alien-and-errno-plusp (alien)
  (and (null-alien alien) (plusp (get-errno))))

;; Slight wart: READDIR returns a null alien at the end of the
;; directory; most other SB-POSIX interfaces (e.g., GETENV, GETPWNAM,
;; GETGRGID, etc) return NIL instead of a null pointer for a non-error
;; return. This might be the only detail in SB-POSIX that makes a user
;; reach for SB-ALIEN, but it'd be an incompatible change to do
;; anything about it.
#+inode64
(define-call ("readdir" :c-name "readdir$INODE64" :options :largefile)
  (* dirent)
  null-alien-and-errno-plusp
  (dir (* t)))
#-inode64
(define-call (#-netbsd "readdir" #+netbsd "_readdir" :options :largefile)
  (* dirent)
  null-alien-and-errno-plusp
  (dir (* t)))
(define-call "closedir" int minusp (dir (* t)))
;; need to do this here because we can't do it in the DEFPACKAGE
(define-call* "umask" mode-t never-fails (mode mode-t))
(define-call* "getpid" pid-t never-fails)

#-win32
(progn
  (define-call "chown" int minusp (pathname filename)
               (owner uid-t) (group gid-t))
  (define-call "chroot" int minusp (pathname filename))
  (define-call "fchdir" int minusp (fd file-descriptor))
  (define-call "fchmod" int minusp (fd file-descriptor) (mode mode-t))
  (define-call "fchown" int minusp (fd file-descriptor)
               (owner uid-t)  (group gid-t))
  (define-call "fdatasync" int minusp (fd file-descriptor))
  (define-call ("ftruncate" :options :largefile)
      int minusp (fd file-descriptor) (length off-t))
  (define-call "fsync" int minusp (fd file-descriptor))
  (define-call "lchown" int minusp (pathname filename)
               (owner uid-t)  (group gid-t))
  (define-call "link" int minusp (oldpath filename) (newpath filename))
  (define-call "lockf" int minusp (fd file-descriptor) (cmd int) (len off-t))
  (define-call "mkfifo" int minusp (pathname filename) (mode mode-t))
  (define-call "symlink" int minusp (oldpath filename) (newpath filename))
  (define-call "sync" void never-fails)
  (define-call ("truncate" :options :largefile)
      int minusp (pathname filename) (length off-t))
  (macrolet ((def-mk*temp (lisp-name c-name result-type errorp dirp values)
               (declare (ignore dirp))
               (if (sb-sys:find-foreign-symbol-address c-name)
                   `(progn
                      (defun ,lisp-name (template)
                        (let* ((external-format sb-alien::*default-c-string-external-format*)
                               (arg (sb-ext:string-to-octets
                                     (filename template)
                                     :external-format external-format
                                     :null-terminate t)))
                          (sb-sys:with-pinned-objects (arg)
                            ;; accommodate for the call-by-reference
                            ;; nature of mks/dtemp's template strings.
                            (let ((result (alien-funcall (extern-alien ,c-name
                                                                       (function ,result-type system-area-pointer))
                                                         (sb-alien::vector-sap arg))))
                              (when (,errorp result)
                                (syscall-error ',lisp-name))
                              ;; FIXME: We'd rather return pathnames, but other
                              ;; SB-POSIX functions like this return strings...
                              (let ((pathname (sb-ext:octets-to-string
                                               arg :external-format external-format
                                               :end (1- (length arg)))))
                                ,(if values
                                     '(values result pathname)
                                     'pathname))))))
                      (export ',lisp-name))
                   `(progn
                      (defun ,lisp-name (template)
                        (declare (ignore template))
                        (unsupported-error ',lisp-name ,c-name))
                      (define-compiler-macro ,lisp-name (&whole form template)
                        (declare (ignore template))
                        (unsupported-warning ',lisp-name ,c-name)
                        form)
                      (export ',lisp-name)))))
    ;; FIXME: The man page for it says "Never use mktemp()"
    (def-mk*temp mktemp "mktemp" (* char) null-alien nil nil)
    ;; FIXME: Windows does have _mktemp, which has a slightly different
    ;; interface
    (def-mk*temp mkstemp "mkstemp" int minusp nil t)
    ;; FIXME: What about Windows?
    (def-mk*temp mkdtemp "mkdtemp" (* char) null-alien t nil))
  (define-call-internally ioctl-without-arg "ioctl" int minusp
                          (fd file-descriptor) (cmd unsigned-long))
  (define-call-internally ioctl-with-int-arg "ioctl" int minusp
                          (fd file-descriptor) (cmd unsigned-long) &optional (arg int))
  (define-call-internally ioctl-with-pointer-arg "ioctl" int minusp
                          (fd file-descriptor) (cmd unsigned-long)
                          &optional
                          (arg alien-pointer-to-anything-or-nil))
  (define-entry-point "ioctl" (fd cmd &optional (arg nil argp))
    (if argp
        (etypecase arg
          ((alien int) (ioctl-with-int-arg fd cmd arg))
          ((or (alien (* t)) null) (ioctl-with-pointer-arg fd cmd arg)))
        (ioctl-without-arg fd cmd)))
  (define-call-internally fcntl-without-arg "fcntl" int minusp
                          (fd file-descriptor) (cmd int))
  (define-call-internally fcntl-with-int-arg "fcntl" int minusp
                          (fd file-descriptor) (cmd int) &optional (arg int))
  (define-call-internally fcntl-with-pointer-arg "fcntl" int minusp
                          (fd file-descriptor) (cmd int)
                          &optional
                          (arg alien-pointer-to-anything-or-nil))
  (define-protocol-class flock alien-flock ()
   ((type :initarg :type :accessor flock-type
          :documentation "Type of lock; F_RDLCK, F_WRLCK, F_UNLCK.")
    (whence :initarg :whence :accessor flock-whence
            :documentation "Flag for starting offset.")
    (start :initarg :start :accessor flock-start
           :documentation "Relative offset in bytes.")
    (len :initarg :len :accessor flock-len
         :documentation "Size; if 0 then until EOF.")
    ;; Note: PID isn't initable, and is read-only.  But other stuff in
    ;; SB-POSIX right now loses when a protocol-class slot is unbound,
    ;; so we initialize it to 0.
    (pid :initform 0 :reader flock-pid
         :documentation
         "Process ID of the process holding the lock; returned with F_GETLK."))
   (:documentation "Class representing locks used in fcntl(2)."))
  (define-entry-point "fcntl" (fd cmd &optional (arg nil argp))
    (if argp
        (etypecase arg
          ((alien int) (fcntl-with-int-arg fd cmd arg))
          ((or (alien (* t)) null) (fcntl-with-pointer-arg fd cmd arg))
          (flock (with-alien-flock a-flock ()
                   (flock-to-alien arg a-flock)
                   (let ((r (fcntl-with-pointer-arg fd cmd a-flock)))
                     (alien-to-flock a-flock arg)
                     r))))
        (fcntl-without-arg fd cmd)))

  ;; uid, gid
  (define-call "geteuid" uid-t never-fails) ; "always successful", it says
  (define-call "getuid" uid-t never-fails)
  (define-call "seteuid" int minusp (uid uid-t))
  #-sunos
  (define-call "setfsuid" int minusp (uid uid-t))
  (define-call "setreuid" int minusp (ruid uid-t) (euid uid-t))
  #-sunos
  (define-call "setresuid" int minusp (ruid uid-t) (euid uid-t) (suid uid-t))
  (define-call "setuid" int minusp (uid uid-t))
  (define-call "getegid" gid-t never-fails)
  (define-call "getgid" gid-t never-fails)
  #-(or sunos darwin)
  (progn
    (export '(getresgid getresuid) :sb-posix)
    (declaim (inline getresgid getresuid))
    (defun getresgid ()
      (with-alien ((rgid gid-t)
                   (egid gid-t)
                   (sgid gid-t))
        (let ((r
                (alien-funcall (extern-alien "getresgid"
                                             (function int (* gid-t) (* gid-t) (* gid-t)))
                               (addr rgid) (addr egid) (addr sgid))))
          (if (minusp r)
              (syscall-error 'getresgid)
              (values rgid egid sgid)))))
    (defun getresuid ()
      (with-alien ((ruid uid-t)
                   (euid uid-t)
                   (suid uid-t))
        (let ((r
                (alien-funcall (extern-alien "getresuid"
                                             (function int (* uid-t) (* uid-t) (* uid-t)))
                               (addr ruid) (addr euid) (addr suid))))
          (if (minusp r)
              (syscall-error 'getresuid)
              (values ruid euid suid))))))
  (define-call "setegid" int minusp (gid gid-t))
  #-sunos
  (define-call "setfsgid" int minusp (gid gid-t))
  (define-call "setgid" int minusp (gid gid-t))
  (define-call "setregid" int minusp (rgid gid-t) (egid gid-t))
  #-sunos
  (define-call "setresgid" int minusp (rgid gid-t) (egid gid-t) (sgid gid-t))

  ;; processes, signals
  (define-call "alarm" int never-fails (seconds unsigned))

  ;; exit and abort, not much point inlining these
  (define-simple-call abort void)
  (define-simple-call exit void (status int))
  (define-simple-call _exit void (status int))

  ;; FIXME this is a lie, of course this can fail, but there's no
  ;; error handling here yet!
  (define-call ("posix_fork" :c-name "fork") pid-t minusp)
  (defun fork ()
    "Forks the current process, returning 0 in the new process and the PID of
the child process in the parent. Forking while multiple threads are running is
not supported."
    ;; It would be easy enough to to allow fork in multithreaded code - we'd need the new
    ;; process to set *ALL-THREADS* to contain only one thread, and unmap other threads'
    ;; stack to avoid a memory leak. The tricky part would be adhering the the POSIX caveats.
    ;; Linux:
    ;;   After a fork() in a multithreaded program, the child can safely call only async-signal-safe
    ;;   functions (see signal-safety(7)) until such time as it calls execve(2).
    ;; FreeBSD:
    ;;   If the process has more than one thread, locks and other resources held by the other
    ;;   threads are not released and therefore only async-signal-safe functions are guaranteed
    ;;   to work in the child process until a call to execve(2) or a similar function.
    ;; macOS:
    ;;   To be totally safe you should restrict yourself to only executing async-signal safe
    ;;   operations until such time as one of the exec functions is called.
    #+sb-thread
    (when (cdr (sb-int:with-system-mutex (sb-thread::*make-thread-lock*)
                 (sb-impl::finalizer-thread-stop)
                 ;; Dead threads aren't pruned from *ALL-THREADS* until the Pthread join.
                 ;; Do that now so that the forked process has only the main thread
                 ;; in *ALL-THREADS* and nothing in *JOINABLE-THREADS*.
                 (sb-thread::%dispose-thread-structs)
                 ;; Threads are added to ALL-THREADS before they have an OS thread,
                 ;; but newborn threads are not exposed in SB-THREAD:LIST-ALL-THREADS.
                 ;; So we need to go lower-level to sense whether any exist.
                 (sb-thread:avltree-list sb-thread::*all-threads*)))
      (sb-impl::finalizer-thread-start)
      (error "Cannot fork with multiple threads running."))
    (let ((pid (posix-fork)))
      (when (= pid 0) ; child
        (alien-funcall (extern-alien "sb_posix_after_fork" (function void))))
      #+sb-thread (sb-impl::finalizer-thread-start)
      pid))
  (export 'fork :sb-posix)

  (define-call "getpgid" pid-t minusp (pid pid-t))
  (define-call "getppid" pid-t never-fails)
  (define-call "getpgrp" pid-t never-fails)
  (define-call "getsid" pid-t minusp  (pid pid-t))
  (define-call "kill" int minusp (pid pid-t) (signal int))
  (define-call "killpg" int minusp (pgrp int) (signal int))
  (define-call "pause" int minusp)
  (define-call "setpgid" int minusp (pid pid-t) (pgid pid-t))
  (define-call "setpgrp" int minusp)
  (define-call "setsid" pid-t minusp))

(defmacro with-growing-c-string ((buffer size) &body body)
  (sb-int:with-unique-names (c-string-block)
    `(block ,c-string-block
       (let (,buffer)
         (flet ((,buffer (&optional (size-incl-null))
                  (when size-incl-null
                    (setf (sb-sys:sap-ref-8 (sb-alien:alien-sap ,buffer) size-incl-null)
                          0))
                  (return-from ,c-string-block
                    (sb-alien::c-string-to-string
                     (sb-alien:alien-sap ,buffer)
                     (sb-impl::default-external-format)
                     'character))))
           (loop for ,size = 128 then (* 2 ,size)
                 do (unwind-protect
                         (progn
                           (setf ,buffer (make-alien c-string ,size))
                           ,@body)
                      (when ,buffer
                        (free-alien ,buffer)))))))))

#-win32
(progn
  (export 'readlink :sb-posix)
  (defun readlink (pathspec)
    "Returns the resolved target of a symbolic link as a string."
    (flet ((%readlink (path buf length)
             (alien-funcall
              (extern-alien "readlink" (function int (c-string :not-null t) (* t) int))
              path buf length)))
      (with-growing-c-string (buf size)
        (let ((count (%readlink (filename pathspec) buf size)))
          (cond ((minusp count)
                 (syscall-error 'readlink))
                ((< 0 count size)
                 (buf count))))))))

(progn
  (export 'getcwd :sb-posix)
  (defun getcwd ()
    "Returns the process's current working directory as a string."
    (flet ((%getcwd (buffer size)
             (alien-funcall
              (extern-alien #-win32 "getcwd"
                            #+win32 "_getcwd" (function c-string (* t) int))
              buffer size)))
      (with-growing-c-string (buf size)
        (let ((result (%getcwd buf size)))
          (cond (result
                 (buf))
                ((/= (get-errno) sb-posix:erange)
                 (syscall-error 'getcwd))))))))

#-win32
(progn
 (export 'wait :sb-posix)
 (declaim (inline wait))
 (defun wait (&optional statusptr)
   (declare (type (or null (simple-array (signed-byte 32) (1))) statusptr))
   (with-alien ((wait (function pid-t (* int)) :extern "wait")
                (status int))
     (let ((pid (alien-funcall wait (addr status))))
       (when (minusp pid) (syscall-error 'wait))
       (when statusptr (setf (aref statusptr 0) status))
       (values pid status)))))

#-win32
(progn
 (export 'waitpid :sb-posix)
 (declaim (inline waitpid))
 (defun waitpid (pid options &optional statusptr)
   (declare (type (sb-alien:alien pid-t) pid)
            (type (sb-alien:alien int) options)
            (type (or null (simple-array (signed-byte 32) (1))) statusptr))
   (with-alien ((waitpid (function pid-t pid-t (* int) int) :extern "waitpid")
                (status int))
     (let ((pid (alien-funcall waitpid pid (addr status) options)))
       (when (minusp pid) (syscall-error 'waitpid))
       (when statusptr (setf (aref statusptr 0) status))
       (values pid status))))
 ;; waitpid macros
 (define-call "wifexited" boolean never-fails (status int))
 (define-call "wexitstatus" int never-fails (status int))
 (define-call "wifsignaled" boolean never-fails (status int))
 (define-call "wtermsig" int never-fails (status int))
 (define-call "wifstopped" boolean never-fails (status int))
 (define-call "wstopsig" int never-fails (status int))
 (define-call "wifcontinued" boolean never-fails (status int)))

;;; mmap, msync
#-win32
(progn
 (define-call ("mmap" :options :largefile) sb-sys:system-area-pointer
   (lambda (res)
     (= (sb-sys:sap-int res) #.(1- (expt 2 sb-vm:n-machine-word-bits))))
   (addr sap-or-nil) (length size-t) (prot unsigned)
   (flags unsigned) (fd file-descriptor) (offset off-t))

 (define-call "munmap" int minusp
   (start sb-sys:system-area-pointer) (length unsigned))

 (define-call "msync" int minusp
   (addr sb-sys:system-area-pointer) (length unsigned) (flags int)))
#+win32
(progn
  ;; No attempt is made to offer a full mmap-like interface on Windows.
  ;; It would be possible to do so (and has been done by AK on his
  ;; branch), but the use case is unclear to me.  However, the following
  ;; definitions are needed to keep existing code in sb-simple-streams
  ;; running. --DFL
  (defconstant PROT-READ #x02)
  (defconstant PROT-WRITE #x04)
  (defconstant PROT-EXEC #x10)
  (defconstant PROT-NONE 0)
  (defconstant MAP-SHARED 0)
  (defconstant MAP-PRIVATE 1)
  (defconstant MS-ASYNC nil)
  (defconstant MS-SYNC nil)
  (export                            ;export on the fly like define-call
   (defun msync (address length flags)
     (declare (ignore flags))
     (when (zerop (sb-win32:flush-view-of-file address length))
       (sb-win32::win32-error "FlushViewOfFile")))))

;;; mlockall, munlockall
(define-call "mlockall" int minusp (flags int))
(define-call "munlockall" int minusp)

(export 'getpagesize)
(declaim (inline getpagesize))
(defun getpagesize () (extern-alien "os_reported_page_size" (unsigned 32)))

;;; passwd database
;; The docstrings are copied from the descriptions in SUSv3,
;; where present.
#-(or android win32)
(define-protocol-class passwd alien-passwd ()
  ((name :initarg :name :accessor passwd-name
         :documentation "User's login name.")
   ;; Note: SUSv3 doesn't require this member.
   (passwd :initarg :passwd :accessor passwd-passwd
           :documentation "The account's encrypted password.")
   (uid :initarg :uid :accessor passwd-uid
        :documentation "Numerical user ID.")
   (gid :initarg :gid :accessor passwd-gid
        :documentation "Numerical group ID.")
   ;; Note: SUSv3 doesn't require this member.
   (gecos :initarg :gecos :accessor passwd-gecos
          :documentation "User's name or comment field.")
   (dir :initarg :dir :accessor passwd-dir
        :documentation "Initial working directory.")
   (shell :initarg :shell :accessor passwd-shell
          :documentation "Program to use as shell."))
  (:documentation
   "Instances of this class represent entries in the system's user database."))

;;; group database
#-(or android win32)
(define-protocol-class group alien-group ()
  ((name :initarg :name :accessor group-name
         :documentation "The name of the group.")
   ;; Note: SUSv4 doesn't require this member
   (passwd :initarg :passwd :accessor group-passwd
           :documentation "The group's encrypted password.")
   (gid :initarg :gid :accessor group-gid
        :documentation "Numerical group ID.")
   (mem :initarg :mem :accessor group-mem
        ;; N.B., omitting any :TO-ALIEN because no alien interface
        ;; reads from a filled-in struct group.
        :from-alien sb-int:c-strings->string-list
        :documentation "A list of strings naming members of the group."))
  (:documentation
   "Instances of this class represent entries in the system's group database."))

;; None of the standardized interfaces to the user or group database
;; is thread-safe or reentrant, at least with respect to getpwent or
;; getgrent. The following two macros are for users to wrap around all
;; getpw* or getgr* calls if they want to do things safely. (Note that
;; on #-sb-thread, this still protects against reentrancy, but doesn't
;; grab a lock.)
#-(or android win32)
(macrolet ((define-database-protection-form (dbname)
  (let* ((macro-name (intern (format nil "WITH-~A-DATABASE" dbname) :sb-posix))
         (lock-var (intern (format nil "*~A-DATABASE-LOCK*" dbname) :sb-posix))
         (lock-name (format nil "~A database lock" dbname))
         (special-var (intern (format nil "*WITH-~A-DATABASE*" dbname) :sb-posix))
         (reentrancy-error-message
          (format nil "~@(~A~) database access is not reentrant." dbname))
         (assertion (intern (format nil "ASSERT-~A" macro-name) :sb-posix)))
    #-sb-thread (declare (ignore lock-var lock-name))
    `(progn
       #+sb-thread
       (sb-ext:defglobal ,lock-var
        (sb-thread:make-mutex :name ,lock-name))
       (defvar ,special-var nil)
       (defmacro ,macro-name (&body body)
         `(progn
            (when ,',special-var
              (error ,',reentrancy-error-message))
            #+sb-thread
            (sb-thread:with-mutex (,',lock-var)
             (let ((,',special-var t))
               ,@body))
            #-sb-thread
            (let ((,',special-var t))
              ,@body)))
       (defmacro ,assertion (function)
         `(unless ,',special-var
            (error "~A may only be called during ~A."
                    ',function ',',macro-name)))))))
(define-database-protection-form passwd)
(define-database-protection-form group))

#-(or win32 android)
(macrolet ((define-obj-call (name result-type conv with-macro assertion
                             &optional arg arg-type)
  ;; FIXME: this isn't the documented way of doing this, surely?
  (let ((lisp-name (intern (string-upcase name) :sb-posix)))
    `(progn
      ;; Don't export GET/SET/END/??ENT bindings; the iteration
      ;; macros are less error-prone.
      ,@(when arg `((export ',lisp-name :sb-posix)))
      (declaim (inline ,lisp-name))
      (defun ,lisp-name (,@(when arg `(,arg)))
        ,@(unless arg
            `((,assertion ',lisp-name)))
        (set-errno 0)
        (let ((r (,@(if arg `(,with-macro) '(progn))
                    (alien-funcall
                     (extern-alien
                      ,name (function ,result-type ,@(when arg-type `(,arg-type))))
                   ,@(when arg `(,arg))))))
          (if (null-alien r)
              (when (plusp (get-errno))
                (syscall-error ',lisp-name))
              (,conv r)))))))
  (define-enumerator-call (name assertion)
      (let ((lisp-name (intern (string-upcase name) :sb-posix)))
        `(progn
           (declaim (inline ,lisp-name))
           (defun ,lisp-name ()
             (,assertion ',lisp-name)
             (alien-funcall (extern-alien ,name (function void))))))))

;; passwd database
(define-obj-call "getpwnam" (* alien-passwd) alien-to-passwd
                 with-passwd-database assert-with-passwd-database
                 login-name (c-string :not-null t))
(define-obj-call "getpwuid" (* alien-passwd) alien-to-passwd
                 with-passwd-database assert-with-passwd-database
                 uid uid-t)
(define-obj-call "getpwent" (* alien-passwd) alien-to-passwd
                 with-passwd-database assert-with-passwd-database)
;; Including these here for thematic grouping.
(define-enumerator-call "setpwent" assert-with-passwd-database)
(define-enumerator-call "endpwent" assert-with-passwd-database)

;; Same thing, but for the group database.
(define-obj-call "getgrnam" (* alien-group) alien-to-group
                 with-group-database assert-with-group-database
                 group-name (c-string :not-null t))
(define-obj-call "getgrgid" (* alien-group) alien-to-group
                 with-group-database assert-with-group-database
                 gid gid-t)
(define-obj-call "getgrent" (* alien-group) alien-to-group
                 with-group-database assert-with-group-database)
(define-enumerator-call "setgrent" assert-with-group-database)
(define-enumerator-call "endgrent" assert-with-group-database)
) ; end MACROLET

#-(or android win32)
(macrolet ((define-database-iterator (dbname with set get end)
  (let* ((name (intern (format nil "DO-~AS" dbname) :sb-posix))
         (docstring
          (let ((*print-right-margin* 70))
            (format
             nil
             "~@<Evaluate BODY with ~A bound to successive entries from ~
              the ~:*~(~A~) database, and return RESULT. ~
              An implicit block named NIL surrounds the form; ~
              an implicit TAGBODY surrounds BODY. ~
              It is unspecified whether ~:*~A is assigned, rebound, or ~
              destructively modified upon each iteration. ~
              It is an error to use any operator that accesses the ~
              ~0@*~A database during the dynamic extent of ~A.~:@>"
             dbname name))))
    `(progn
       (export ',name :sb-posix)
       (defmacro ,name ((,dbname &optional result) &body body)
         ,docstring
         `(,',with
           (,',set)
           (do ((,,dbname (,',get) (,',get)))
               ((null ,,dbname) (,',end) ,result)
             ,@body)))))))
  (define-database-iterator passwd with-passwd-database
                            setpwent getpwent endpwent)
  (define-database-iterator group with-group-database
                            setgrent getgrent endgrent))

#-win32
(define-protocol-class timeval alien-timeval ()
  ((sec :initarg :tv-sec :accessor timeval-sec
        :documentation "Seconds.")
   (usec :initarg :tv-usec :accessor timeval-usec
         :documentation "Microseconds."))
  (:documentation "Instances of this class represent time values."))

(define-protocol-class stat alien-stat ()
  ((mode :initarg :mode :reader stat-mode
         :documentation "Mode of file.")
   (ino :initarg :ino :reader stat-ino
        :documentation "File serial number.")
   (dev :initarg :dev :reader stat-dev
        :documentation "Device ID of device containing file.")
   (nlink :initarg :nlink :reader stat-nlink
          :documentation "Number of hard links to the file.")
   (uid :initarg :uid :reader stat-uid
        :documentation "User ID of file.")
   (gid :initarg :gid :reader stat-gid
        :documentation "Group ID of file.")
   (size :initarg :size :reader stat-size
         :documentation "For regular files, the file size in
                         bytes.  For symbolic links, the length
                         in bytes of the filename contained in
                         the symbolic link.")
   (rdev :initarg :rdev :reader stat-rdev
          :documentation "For devices the device number.")
   (atime :initarg :atime :reader stat-atime
          :documentation "Time of last access.")
   (mtime :initarg :mtime :reader stat-mtime
          :documentation "Time of last data modification.")
   (ctime :initarg :ctime :reader stat-ctime
          :documentation "Time of last status change."))
  (:documentation "Instances of this class represent POSIX file metadata."))

(defmacro define-stat-call (name arg designator-fun type)
  ;; FIXME: this isn't the documented way of doing this, surely?
  (let ((lisp-name (lisp-for-c-symbol name))
        (real-name #+inode64 (format nil "~A$INODE64" name)
                   #-inode64 name))
    `(progn
      (export ',lisp-name :sb-posix)
      (declaim (inline ,lisp-name))
      (defun ,lisp-name (,arg &optional stat)
        (declare (type (or null stat) stat))
        (with-alien-stat a-stat ()
          (let ((r (alien-funcall
                    (extern-alien ,(real-c-name (list real-name :options :largefile)) ,type)
                    (,designator-fun ,arg)
                    a-stat)))
            (when (minusp r)
              (syscall-error ',lisp-name))
            (alien-to-stat a-stat stat)))))))

(define-stat-call #-win32 "stat" #+win32 "_stat"
                  pathname filename
                  (function int (c-string :not-null t) (* alien-stat)))

#-win32
(define-stat-call "lstat"
                  pathname filename
                  (function int (c-string :not-null t) (* alien-stat)))
;;; No symbolic links on Windows, so use stat
#+win32
(progn
  (declaim (inline lstat))
  (export (defun lstat (filename &optional stat)
            (if stat (stat filename stat) (stat filename)))))

(define-stat-call #-win32 "fstat" #+win32 "_fstat"
                  fd file-descriptor
                  (function int int (* alien-stat)))


;;; mode flags
(define-call "s_isreg" boolean never-fails (mode mode-t))
(define-call "s_isdir" boolean never-fails (mode mode-t))
(define-call "s_ischr" boolean never-fails (mode mode-t))
(define-call "s_isblk" boolean never-fails (mode mode-t))
(define-call "s_isfifo" boolean never-fails (mode mode-t))
(define-call "s_islnk" boolean never-fails (mode mode-t))
(define-call "s_issock" boolean never-fails (mode mode-t))

#-win32
(progn
 (export 'pipe :sb-posix)
 (declaim (inline pipe))
 (defun pipe (&optional filedes2)
   (declare (type (or null (simple-array (signed-byte 32) (2))) filedes2))
   (multiple-value-bind (fd0 fd1)
       (with-alien ((pipe (function int (array int 2)) :extern "pipe")
                    (fds (array int 2)))
         (let ((r (alien-funcall pipe fds)))
           (if (minusp r) (syscall-error 'pipe) (values (deref fds 0) (deref fds 1)))))
     (when filedes2 (setf (aref filedes2 0) fd0 (aref filedes2 1) fd1))
     (values fd0 fd1))))

#-win32
(define-protocol-class termios alien-termios ()
  ((iflag :initarg :iflag :accessor sb-posix:termios-iflag
          :documentation "Input modes.")
   (oflag :initarg :oflag :accessor sb-posix:termios-oflag
          :documentation "Output modes.")
   (cflag :initarg :cflag :accessor sb-posix:termios-cflag
          :documentation "Control modes.")
   (lflag :initarg :lflag :accessor sb-posix:termios-lflag
          :documentation "Local modes.")
   (cc :initarg :cc :accessor sb-posix:termios-cc :array-length nccs
       :documentation "Control characters."))
  (:documentation
   "Instances of this class represent I/O characteristics of the terminal."))

#-win32
(progn
 (export 'tcsetattr :sb-posix)
 (declaim (inline tcsetattr))
 (defun tcsetattr (fd actions termios)
   (declare (type termios termios))
   (with-alien-termios a-termios ()
     (termios-to-alien termios a-termios)
     (let ((fd (file-descriptor fd)))
       (let* ((r (alien-funcall
                  (extern-alien
                   "tcsetattr"
                   (function int int int (* alien-termios)))
                  fd actions a-termios)))
         (when (minusp r)
           (syscall-error 'tcsetattr)))
       (values))))
 (export 'tcgetattr :sb-posix)
 (declaim (inline tcgetattr))
 (defun tcgetattr (fd &optional termios)
   (declare (type (or null termios) termios))
   (with-alien-termios a-termios ()
     (let ((r (alien-funcall
               (extern-alien "tcgetattr"
                             (function int int (* alien-termios)))
               (file-descriptor fd)
               a-termios)))
       (when (minusp r)
         (syscall-error 'tcgetattr))
       (setf termios (alien-to-termios a-termios termios))))
   termios)
 (define-call "tcdrain" int minusp (fd file-descriptor))
 (define-call "tcflow" int minusp (fd file-descriptor) (action int))
 (define-call "tcflush" int minusp (fd file-descriptor) (queue-selector int))
 (define-call "tcgetsid" pid-t minusp (fd file-descriptor))
 (define-call "tcsendbreak" int minusp (fd file-descriptor) (duration int))
 (export 'cfsetispeed :sb-posix)
 (declaim (inline cfsetispeed))
 (defun cfsetispeed (speed &optional termios)
   (declare (type (or null termios) termios))
   (with-alien-termios a-termios ()
     (termios-to-alien termios a-termios)
     (let ((r (alien-funcall
               (extern-alien "cfsetispeed"
                             (function int (* alien-termios) speed-t))
               a-termios
               speed)))
       (when (minusp r)
         (syscall-error 'cfsetispeed))
       (setf termios (alien-to-termios a-termios termios))))
   termios)
 (export 'cfsetospeed :sb-posix)
 (declaim (inline cfsetospeed))
 (defun cfsetospeed (speed &optional termios)
   (declare (type (or null termios) termios))
   (with-alien-termios a-termios ()
     (termios-to-alien termios a-termios)
     (let ((r (alien-funcall
               (extern-alien "cfsetospeed"
                             (function int (* alien-termios) speed-t))
               a-termios
               speed)))
       (when (minusp r)
         (syscall-error 'cfsetospeed))
       (setf termios (alien-to-termios a-termios termios))))
   termios)
 (export 'cfgetispeed :sb-posix)
 (declaim (inline cfgetispeed))
 (defun cfgetispeed (termios)
   (declare (type termios termios))
   (with-alien-termios a-termios ()
     (termios-to-alien termios a-termios)
     (alien-funcall (extern-alien "cfgetispeed"
                                  (function speed-t (* alien-termios)))
                    a-termios)))
 (export 'cfgetospeed :sb-posix)
 (declaim (inline cfgetospeed))
 (defun cfgetospeed (termios)
   (declare (type termios termios))
   (with-alien-termios a-termios ()
     (termios-to-alien termios a-termios)
     (alien-funcall (extern-alien "cfgetospeed"
                                 (function speed-t (* alien-termios)))
                    a-termios))))


#-win32
(progn
  (export 'time :sb-posix)
  (defun time ()
    (let ((result (alien-funcall (extern-alien "time"
                                               (function time-t (* time-t)))
                                 nil)))
      (if (minusp result)
          (syscall-error 'time)
          result)))
  (export 'utime :sb-posix)
  (defun utime (filename &optional access-time modification-time)
    (let ((fun (extern-alien #-netbsd "utime" #+netbsd "_utime"
                             (function int (c-string :not-null t)
                                       (* alien-utimbuf))))
          (name (filename filename)))
      (if (not (and access-time modification-time))
          (alien-funcall fun name nil)
          (with-alien ((utimbuf (struct alien-utimbuf)))
            (setf (slot utimbuf 'actime) (or access-time 0)
                  (slot utimbuf 'modtime) (or modification-time 0))
            (let ((result (alien-funcall fun name (alien-sap utimbuf))))
              (if (minusp result)
                  (syscall-error 'utime)
                  result))))))
  (export 'utimes :sb-posix)
  (defun utimes (filename &optional access-time modification-time)
    (flet ((seconds-and-useconds (time)
             (multiple-value-bind (integer fractional)
                 (cl:truncate time)
               (values integer (cl:truncate (* fractional 1000000)))))
           (maybe-syscall-error (value)
             (if (minusp value)
                 (syscall-error 'utimes)
                 value)))
      (let ((fun (extern-alien #-netbsd "utimes" #+netbsd "sb_utimes"
                               (function int (c-string :not-null t)
                                                  (* (array alien-timeval 2)))))
            (name (filename filename)))
        (if (not (and access-time modification-time))
            (maybe-syscall-error (alien-funcall fun name nil))
            (with-alien ((buf (array alien-timeval 2)))
              (let ((actime (deref buf 0))
                    (modtime (deref buf 1)))
                (setf (values (slot actime 'sec)
                              (slot actime 'usec))
                      (seconds-and-useconds (or access-time 0))
                      (values (slot modtime 'sec)
                              (slot modtime 'usec))
                      (seconds-and-useconds (or modification-time 0)))
                (maybe-syscall-error (alien-funcall fun name
                                                    (alien-sap buf))))))))))


;;; environment

(defun getenv (name)
  ;; SUSv4 doesn't define any errors for getenv, but some systems do.
  (set-errno 0)
  (let ((r (alien-funcall
            (extern-alien "getenv" (function (* char) (c-string :not-null t)))
            name)))
    (declare (type (alien (* char)) r))
    (if (null-alien r)
        (when (plusp (get-errno))
          (syscall-error 'getenv))
        (cast r c-string))))
#-win32
(progn
  (define-call "setenv" int minusp
               (name (c-string :not-null t))
               (value (c-string :not-null t))
               (overwrite int))
  (define-call "unsetenv" int minusp (name (c-string :not-null t)))
  (export 'putenv :sb-posix)
  (defun putenv (string)
    (declare (string string))
    ;; We don't want to call actual putenv: the string passed to putenv ends
    ;; up in environ, and we any string we allocate GC might move.
    ;;
    ;; This makes our wrapper nonconformant if you squit hard enough, but
    ;; users who care about that should really be calling putenv() directly in
    ;; order to be able to manage memory sanely.
    (let ((p (position #\= string))
          (n (length string)))
      (if p
          (if (= p n)
              (unsetenv (subseq string 0 p))
              (setenv (subseq string 0 p) (subseq string (1+ p)) 1))
          (error "Invalid argument to putenv: ~S" string)))))
#+win32
(progn
  ;; Windows doesn't define a POSIX setenv, but happily their _putenv is sane.
  (define-call* "putenv" int minusp (string (c-string :not-null t)))
  (export 'setenv :sb-posix)
  (defun setenv (name value overwrite)
    (declare (string name value))
    (if (and (zerop overwrite) (sb-posix:getenv name))
        0
        (putenv (concatenate 'string name "=" value))))
  (export 'unsetenv :sb-posix)
  (defun unsetenv (name)
    (declare (string name))
    (putenv (concatenate 'string name "="))))

;;; syslog
#-win32
(progn
  (export 'openlog :sb-posix)
  (export 'syslog :sb-posix)
  (export 'closelog :sb-posix)
  (defun openlog (ident options &optional (facility log-user))
    (alien-funcall (extern-alien
                    "openlog" (function void (c-string :not-null t) int int))
                   ident options facility))
  (defun syslog (priority format &rest args)
    "Send a message to the syslog facility, with severity level
PRIORITY.  The message will be formatted as by CL:FORMAT (rather
than C's printf) with format string FORMAT and arguments ARGS."
    (flet ((syslog1 (priority message)
             (alien-funcall (extern-alien
                             "syslog" (function void int
                                                (c-string :not-null t)
                                                (c-string :not-null t)))
                            priority "%s" message)))
      (syslog1 priority (apply #'format nil format args))))
  (define-call "closelog" void never-fails))
