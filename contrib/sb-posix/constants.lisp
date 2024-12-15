;;; -*- Lisp -*-

;;; This isn't really lisp, but it's definitely a source file.

;;; first, the headers necessary to find definitions of everything
(#||#
 "sys/types.h"
 "sys/stat.h"
 #+haiku "sys/time.h"
 #-win32 "utime.h"
 #-win32 "sys/socket.h"
 #-win32 "sys/un.h"
 #-win32 "netinet/in.h"
 #-win32 "netinet/in_systm.h"
 #-win32 "netinet/ip.h"
 #-win32 "net/if.h"
 #-win32 "netinet/tcp.h"
 #-win32 "sys/mman.h"
 #-win32 "sys/wait.h"
 "fcntl.h"
 #-win32 "netdb.h"
 "errno.h"
 "dirent.h" "signal.h"
 #-(or win32 android) "pwd.h"
 #-(or win32 android) "grp.h"
 "unistd.h"
 #-win32 "termios.h"
 #-win32 "syslog.h")

;;; then the stuff we're looking for
((:integer af-inet "AF_INET" "IP Protocol family" t)

 ;; KLUDGE: These types simply do not seem to exist on Windows,
 ;; but we'll provide these anyways -- at least in a way that should
 ;; match with stat.
 (:type uid-t   #-win32 "uid_t"   #+win32 "short")
 (:type gid-t   #-win32 "gid_t"   #+win32 "short")
 (:type nlink-t #-win32 "nlink_t" #+win32 "short")

 (:type pid-t "pid_t")
 (:type ino-t "ino_t")

 (:type time-t "time_t")
 (:type dev-t "dev_t")

 ;; signals
 (:integer SIGHUP "SIGHUP" "terminal line hangup." t)
 (:integer SIGINT "SIGINT" "interrupt program." t)
 (:integer SIGQUIT "SIGQUIT" "quit program." t)
 (:integer SIGILL "SIGILL" "illegal instruction." t)
 (:integer SIGTRAP "SIGTRAP" "trace trap." t)
 (:integer SIGABRT "SIGABRT" "abort program (formerly SIGIOT)." t)
 (:integer SIGEMT "SIGEMT" "emulate instruction executed." t)
 (:integer SIGFPE "SIGFPE" "floating-point exception." t)
 (:integer SIGKILL "SIGKILL" "kill program." t)
 (:integer SIGBUS "SIGBUS" "bus error." t)
 (:integer SIGSEGV "SIGSEGV" "segmentation violation." t)
 (:integer SIGSYS "SIGSYS" "non-existent system call invoked." t)
 (:integer SIGPIPE "SIGPIPE" "write on a pipe with no reader." t)
 (:integer SIGALRM "SIGALRM" "real-time timer expired." t)
 (:integer SIGTERM "SIGTERM" "software termination signal." t)
 (:integer SIGURG "SIGURG" "urgent condition present on socket." t)
 (:integer SIGSTOP "SIGSTOP" "stop (cannot be caught or ignored)." t)
 (:integer SIGTSTP "SIGTSTP" "stop signal generated from keyboard." t)
 (:integer SIGCONT "SIGCONT" "continue after stop." t)
 (:integer SIGCHLD "SIGCHLD" "child status has changed." t)
 (:integer SIGTTIN "SIGTTIN"
           "background read attempted from control terminal." t)
 (:integer SIGTTOU "SIGTTOU"
           "background write attempted to control terminal." t)
 #-haiku (:integer SIGIO "SIGIO"
           "I/O is possible on a descriptor (see fcntl(2))." t)
 (:integer SIGXCPU "SIGXCPU"
           "cpu time limit exceeded (see setrlimit(2))." t)
 (:integer SIGXFSZ "SIGXFSZ"
           "file size limit exceeded (see setrlimit(2))." t)
 (:integer SIGVTALRM "SIGVTALRM"
           "virtual time alarm (see setitimer(2))." t)
 (:integer SIGPROF "SIGPROF"
           "profiling timer alarm (see setitimer(2))." t)
 (:integer SIGWINCH "SIGWINCH" "Window size change." t)
 (:integer SIGPWR "SIGPWR" "Power failure." t)
 (:integer SIGUSR1 "SIGUSR1" "User defined signal 1." t)
 (:integer SIGUSR2 "SIGUSR2" "User defined signal 2." t)
 (:integer SIGRTMIN "SIGRTMIN" "Smallest real-time signal number." t)
 (:integer SIGRTMAX "SIGRTMAX" "Largest real-time signal number." t)

 ;; error numbers
 (:errno eperm "EPERM" nil t)
 (:errno enoent "ENOENT" nil t)
 (:errno esrch "ESRCH" nil t)
 (:errno eintr "EINTR" nil t)
 (:errno eio "EIO" nil t)
 (:errno enxio "ENXIO" nil t)
 (:errno e2big "E2BIG" nil t)
 (:errno enoexec "ENOEXEC" nil t)
 (:errno ebadf "EBADF" nil t)
 (:errno echild "ECHILD" nil t)
 (:errno eagain "EAGAIN" nil t)
 (:errno enomem "ENOMEM" nil t)
 (:errno eacces "EACCES" nil t)
 (:errno efault "EFAULT" nil t)
 (:errno enotblk "ENOTBLK" nil t)
 (:errno ebusy "EBUSY" nil t)
 (:errno eexist "EEXIST" nil t)
 (:errno exdev "EXDEV" nil t)
 (:errno enodev "ENODEV" nil t)
 (:errno enotdir "ENOTDIR" nil t)
 (:errno eisdir "EISDIR" nil t)
 (:errno einval "EINVAL" nil t)
 (:errno enfile "ENFILE" nil t)
 (:errno emfile "EMFILE" nil t)
 (:errno enotty "ENOTTY" nil t)
 (:errno etxtbsy "ETXTBSY" nil t)
 (:errno efbig "EFBIG" nil t)
 (:errno enospc "ENOSPC" nil t)
 (:errno espipe "ESPIPE" nil t)
 (:errno erofs "EROFS" nil t)
 (:errno emlink "EMLINK" nil t)
 (:errno epipe "EPIPE" nil t)
 (:errno edom "EDOM" nil t)
 (:errno erange "ERANGE" nil t)
 (:errno edeadlk "EDEADLK" nil t)
 (:errno enametoolong "ENAMETOOLONG" nil t)
 (:errno enolck "ENOLCK" nil t)
 (:errno enosys "ENOSYS" nil t)
 (:errno enotempty "ENOTEMPTY" nil t)
 (:errno eloop "ELOOP" nil t)
 (:errno ewouldblock "EWOULDBLOCK" nil t)
 (:errno enomsg "ENOMSG" nil t)
 (:errno eidrm "EIDRM" nil t)
 (:errno echrng "ECHRNG" nil t)
 (:errno el2nsync "EL2NSYNC" nil t)
 (:errno el3hlt "EL3HLT" nil t)
 (:errno el3rst "EL3RST" nil t)
 (:errno elnrng "ELNRNG" nil t)
 (:errno eunatch "EUNATCH" nil t)
 (:errno enocsi "ENOCSI" nil t)
 (:errno el2hlt "EL2HLT" nil t)
 (:errno ebade "EBADE" nil t)
 (:errno ebadr "EBADR" nil t)
 (:errno exfull "EXFULL" nil t)
 (:errno enoano "ENOANO" nil t)
 (:errno ebadrqc "EBADRQC" nil t)
 (:errno ebadslt "EBADSLT" nil t)
 (:errno edeadlock "EDEADLOCK" nil t)
 (:errno ebfont "EBFONT" nil t)
 (:errno enostr "ENOSTR" nil t)
 (:errno enodata "ENODATA" nil t)
 (:errno etime "ETIME" nil t)
 (:errno enosr "ENOSR" nil t)
 (:errno enonet "ENONET" nil t)
 (:errno enopkg "ENOPKG" nil t)
 (:errno eremote "EREMOTE" nil t)
 (:errno enolink "ENOLINK" nil t)
 (:errno eadv "EADV" nil t)
 (:errno esrmnt "ESRMNT" nil t)
 (:errno ecomm "ECOMM" nil t)
 (:errno eproto "EPROTO" nil t)
 (:errno emultihop "EMULTIHOP" nil t)
#-sunos (:errno edotdot "EDOTDOT" nil t)
 (:errno ebadmsg "EBADMSG" nil t)
 (:errno eoverflow "EOVERFLOW" nil t)
 (:errno enotuniq "ENOTUNIQ" nil t)
 (:errno ebadfd "EBADFD" nil t)
 (:errno eremchg "EREMCHG" nil t)
 (:errno elibacc "ELIBACC" nil t)
 (:errno elibbad "ELIBBAD" nil t)
 (:errno elibscn "ELIBSCN" nil t)
 (:errno elibmax "ELIBMAX" nil t)
 (:errno elibexec "ELIBEXEC" nil t)
 (:errno eilseq "EILSEQ" nil t)
 (:errno erestart "ERESTART" nil t)
 (:errno estrpipe "ESTRPIPE" nil t)
 (:errno eusers "EUSERS" nil t)
 (:errno enotsock "ENOTSOCK" nil t)
 (:errno edestaddrreq "EDESTADDRREQ" nil t)
 (:errno emsgsize "EMSGSIZE" nil t)
 (:errno eprototype "EPROTOTYPE" nil t)
 (:errno enoprotoopt "ENOPROTOOPT" nil t)
 (:errno eprotonosupport "EPROTONOSUPPORT" nil t)
 (:errno esocktnosupport "ESOCKTNOSUPPORT" nil t)
 (:errno eopnotsupp "EOPNOTSUPP" nil t)
 (:errno epfnosupport "EPFNOSUPPORT" nil t)
 (:errno eafnosupport "EAFNOSUPPORT" nil t)
 (:errno eaddrinuse "EADDRINUSE" nil t)
 (:errno eaddrnotavail "EADDRNOTAVAIL" nil t)
 (:errno enetdown "ENETDOWN" nil t)
 (:errno enetunreach "ENETUNREACH" nil t)
 (:errno enetreset "ENETRESET" nil t)
 (:errno econnaborted "ECONNABORTED" nil t)
 (:errno econnreset "ECONNRESET" nil t)
 (:errno enobufs "ENOBUFS" nil t)
 (:errno eisconn "EISCONN" nil t)
 (:errno enotconn "ENOTCONN" nil t)
 (:errno eshutdown "ESHUTDOWN" nil t)
 (:errno etoomanyrefs "ETOOMANYREFS" nil t)
 (:errno etimedout "ETIMEDOUT" nil t)
 (:errno econnrefused "ECONNREFUSED" nil t)
 (:errno ehostdown "EHOSTDOWN" nil t)
 (:errno ehostunreach "EHOSTUNREACH" nil t)
 (:errno ealready "EALREADY" nil t)
 (:errno einprogress "EINPROGRESS" nil t)
 (:errno estale "ESTALE" nil t)
#-sunos (:errno euclean "EUCLEAN" nil t)
#-sunos (:errno enotnam "ENOTNAM" nil t)
#-sunos (:errno enavail "ENAVAIL" nil t)
#-sunos (:errno eremoteio "EREMOTEIO" nil t)
#-sunos (:errno edquot "EDQUOT" nil t)
#-sunos (:errno enomedium "ENOMEDIUM" nil t)
#-sunos (:errno emediumtype "EMEDIUMTYPE" nil t)

 ;; wait
 (:integer wnohang "WNOHANG" nil t)
 (:integer wuntraced "WUNTRACED" nil t)

 ;; mode_t
 (:type mode-t "mode_t")
 (:integer s-ifmt "S_IFMT" nil t)
 (:integer s-ififo "S_IFIFO" nil t)
 (:integer s-ifchr "S_IFCHR" nil t)
 (:integer s-ifdir "S_IFDIR" nil t)
 (:integer s-ifblk "S_IFBLK" nil t)
 (:integer s-ifreg "S_IFREG" nil t)
 (:integer s-iflnk "S_IFLNK" nil t)
 (:integer s-ifsock "S_IFSOCK" nil t)
#-sunos (:integer s-ifwht "S_IFWHT" nil t)
 (:integer s-isuid "S_ISUID" nil t)
 (:integer s-isgid "S_ISGID" nil t)
 (:integer s-isvtx "S_ISVTX" nil t)
 (:integer s-irusr "S_IRUSR" nil t)
 (:integer s-iwusr "S_IWUSR" nil t)
 (:integer s-ixusr "S_IXUSR" nil t)
 (:integer s-iread "S_IRUSR" nil t)
 (:integer s-iwrite "S_IWUSR" nil t)
 (:integer s-iexec "S_IXUSR" nil t)
 (:integer s-irgrp "S_IRGRP" nil t)
 (:integer s-iwgrp "S_IWGRP" nil t)
 (:integer s-ixgrp "S_IXGRP" nil t)
 (:integer s-iroth "S_IROTH" nil t)
 (:integer s-iwoth "S_IWOTH" nil t)
 (:integer s-ixoth "S_IXOTH" nil t)

 ;; access()
 (:integer r-ok "R_OK" nil t)
 (:integer w-ok "W_OK" nil t)
 (:integer x-ok "X_OK" nil t)
 (:integer f-ok "F_OK" nil t)

 ;; mmap()
 (:type off-t "off_t")
 (:integer prot-none "PROT_NONE" "mmap: no protection" t)
 (:integer prot-read "PROT_READ" "mmap: read protection" t)
 (:integer prot-write "PROT_WRITE" "mmap: write protection" t)
 (:integer prot-exec "PROT_EXEC" "mmap: execute protection" t)
 (:integer map-shared "MAP_SHARED" "mmap: shared memory" t)
 (:integer map-private "MAP_PRIVATE" "mmap: private mapping" t)
 (:integer map-fixed "MAP_FIXED" "mmap: map at given location" t)
 (:integer map-anon "MAP_ANON" "mmap: anonymous mapping not associated with any file" t)

 ;; msync()
 (:integer ms-async "MS_ASYNC" "msync: return immediately" t)
 (:integer ms-sync "MS_SYNC" "msync: perform synchronous writes" t)
 (:integer ms-invalidate "MS_INVALIDATE"
           "msync: invalidate all cached data" t)

 ;; mlockall()
 (:integer mcl-current "MCL_CURRENT" "mlockall: lock all pages which are currently mapped into the address space of the process." t)
 (:integer mcl-future "MCL_FUTURE" "mlockall: lock all pages which will become mapped into the address space of the process in the future." t)

 ;; opendir()
 (:structure dirent
             (#+(and linux largefile) "struct dirent64"
              #-(and linux largefile) "struct dirent"
              #-(or win32 android) (:ino-t ino "ino_t" "d_ino")
              #+android ((unsigned 64) ino "unsigned long long" "d_ino")
              #-haiku
              (:c-string name "char *" "d_name"
                         ;; FIXME: sunos should really have :distrust-length
                         ;; t, but this is currently broken. -- Jim Wise 2010-08-31
                         :distrust-length nil)) t)

 ;; password database
 #-(or android win32)
 (:structure alien-passwd
             ("struct passwd"
              (c-string-pointer name "char *" "pw_name")
              (c-string-pointer passwd "char *" "pw_passwd")
              (uid-t uid "uid_t" "pw_uid")
              (gid-t gid "gid_t" "pw_gid")
              ;; 'change', 'class', and 'expire' are not supported on Linux
              #+nil
              (time-t change "time_t" "pw_change")
              #+nil
              (c-string-pointer class "char *" "pw_class")
              (c-string-pointer gecos "char *" "pw_gecos")
              (c-string-pointer dir "char *" "pw_dir")
              (c-string-pointer shell "char *" "pw_shell")
              #+nil
              (time-t expire "time_t" "pw_expire")
              ;; OS X manpages say this exists.  they lie!
              #+nil
              (:integer fields "int" "pw_fields")))

 ;; group database
 #-(or android win32)
 (:structure alien-group
             ("struct group"
              (c-string-pointer name "char *" "gr_name")
              (c-string-pointer passwd "char *" "gr_passwd")
              (gid-t gid "gid_t" "gr_gid")
              ((* c-string) mem "char **" "gr_mem")))

 (:structure alien-stat
             ("struct stat"
              (mode-t mode "mode_t" "st_mode")
              #-android
              (ino-t ino "ino_t" "st_ino")
              #+android
              ((unsigned 64) ino "unsigned long long" "st_ino")
              ;; Linux/MIPS uses unsigned long instead of dev_t here.
              #-(or mips android)
              (dev-t dev "dev_t" "st_dev")
              #+mips
              ((unsigned 32) dev "dev_t" "st_dev")
              #+android
              ((unsigned 64) dev "unsigned long long" "st_dev")
              (nlink-t nlink "nlink_t" "st_nlink")
              (uid-t uid "uid_t" "st_uid")
              ;; Linux/MIPS uses unsigned long instead of dev_t here.
              #-(or mips android)
              (dev-t rdev "dev_t" "st_rdev")
              #+mips
              ((unsigned 32) rdev "dev_t" "st_rdev")
              #+android
              ((unsigned 64) rdev "unsigned long long"  "st_rdev")
              (gid-t gid "gid_t" "st_gid")
              #-android
              (off-t size "off_t" "st_size")
              #+android
              ((signed 64) size "long long" "st_size")
              (time-t atime "time_t" "st_atime")
              (time-t mtime "time_t" "st_mtime")
              (time-t ctime "time_t" "st_ctime")))

 #+darwin
 (:structure alien-timespec
             ("struct timespec"
              (time-t tv-sec "time_t" "tv_sec")
              (long tv-nsec "long" "tv_nsec")))

 ;; open()
 (:integer o-rdonly "O_RDONLY" nil t)
 (:integer o-wronly "O_WRONLY" nil t)
 (:integer o-rdwr "O_RDWR" nil t)
 (:integer o-creat "O_CREAT" nil t)
 (:integer o-excl "O_EXCL" nil t)
 (:integer o-noctty "O_NOCTTY" nil t)
 (:integer o-trunc "O_TRUNC" nil t)
 (:integer o-append "O_APPEND" nil t)
 (:integer o-nonblock "O_NONBLOCK" nil t)
 (:integer o-ndelay "O_NDELAY" nil t)
 (:integer o-sync "O_SYNC" nil t)
 (:integer o-nofollow "O_NOFOLLOW" nil t)
#-sunos (:integer o-directory "O_DIRECTORY" nil t)
#-sunos (:integer o-direct "O_DIRECT" nil t)
#-sunos (:integer o-async "O_ASYNC" nil t)
 (:integer o-largefile "O_LARGEFILE" nil t)     ; hmm...
 (:integer o-dsync "O_DSYNC" nil t)
 (:integer o-rsync "O_RSYNC" nil t)

 ;; lseek()
 (:integer seek-set "SEEK_SET" nil t)
 (:integer seek-cur "SEEK_CUR" nil t)
 (:integer seek-end "SEEK_END" nil t)

 ;; fcntl()
 (:integer f-dupfd "F_DUPFD" nil t)
 (:integer f-getfd "F_GETFD" nil t)
 (:integer f-setfd "F_SETFD" nil t)
 (:integer f-getfl "F_GETFL" nil t)
 (:integer f-setfl "F_SETFL" nil t)
 (:integer f-getlk "F_GETLK" nil t)
 (:integer f-setlk "F_SETLK" nil t)
 (:integer f-setlkw "F_SETLKW" nil t)
 (:integer f-getown "F_GETOWN" nil t)
 (:integer f-setown "F_SETOWN" nil t)
 (:integer f-rdlck "F_RDLCK" nil t)
 (:integer f-wrlck "F_WRLCK" nil t)
 (:integer f-unlck "F_UNLCK" nil t)

 #-win32
 (:structure alien-flock
             ("struct flock"
              (short type "short" "l_type")
              (short whence "short" "l_whence")
              (off-t start "off_t" "l_start")
              (off-t len "off_t" "l_len")
              (pid-t pid "pid_t" "l_pid")))

 ;; lockf()
 (:integer f-lock "F_LOCK" nil t)
 (:integer f-tlock "F_TLOCK" nil t)
 (:integer f-ulock "F_ULOCK" nil t)
 (:integer f-test "F_TEST" nil t)

 ;; tcgetattr(), tcsetattr()
 #-win32
 (:type cc-t "cc_t")
 #-win32
 (:type speed-t "speed_t" nil t)
 #-win32
 (:type tcflag-t "tcflag_t" nil t)
 (:integer nccs "NCCS" nil t)
 #-win32
 (:structure alien-termios
             ("struct termios"
              (tcflag-t iflag "tcflag_t" "c_iflag")
              (tcflag-t oflag "tcflag_t" "c_oflag")
              (tcflag-t cflag "tcflag_t" "c_cflag")
              (tcflag-t lflag "tcflag_t" "c_lflag")
              ((array cc-t) cc "cc_t" "c_cc")))

 ;; utime(), utimes()
 #-win32
 (:type suseconds-t ; OAOOM warning: similar kludge in tools-for-build
        #+os-provides-suseconds-t "suseconds_t"
        #-os-provides-suseconds-t "long")

 #-win32
 (:structure alien-utimbuf
             ("struct utimbuf"
              (time-t actime "time_t" "actime")
              (time-t modtime "time_t" "modtime")))
 #-win32
 (:structure alien-timeval
             ("struct timeval"
              (time-t sec "time_t" "tv_sec")
              (suseconds-t usec "suseconds_t" "tv_usec")))

 (:integer veof "VEOF" nil t)
 (:integer veol "VEOL" nil t)
 (:integer verase "VERASE" nil t)
 (:integer vintr "VINTR" nil t)
 (:integer vkill "VKILL" nil t)
 (:integer vmin "VMIN" nil t)
 (:integer vquit "VQUIT" nil t)
 (:integer vstart "VSTART" nil t)
 (:integer vstop "VSTOP" nil t)
 (:integer vsusp "VSUSP" nil t)
 (:integer vtime "VTIME" nil t)
 (:integer vdisable "_POSIX_VDISABLE" nil t)

 (:integer brkint "BRKINT" nil t)
 (:integer icrnl "ICRNL" nil t)
 (:integer ignbrk "IGNBRK" nil t)
 (:integer igncr "IGNCR" nil t)
 (:integer ignpar "IGNPAR" nil t)
 (:integer inlcr "INLCR" nil t)
 (:integer inpck "INPCK" nil t)
 (:integer istrip "ISTRIP" nil t)
 #+xsi                               ; FIXME: an extension, apparently
 (:integer ixany "IXANY" nil t)
 (:integer ixoff "IXOFF" nil t)
 (:integer ixon "IXON" nil t)
 (:integer parmrk "PARMRK" nil t)

 (:integer opost "OPOST" nil t)
 #+xsi
 (:integer onlcr "ONLCR" nil t)
 (:integer ocrnl "OCRNL" nil t)
 (:integer onocr "ONOCR" nil t)
 (:integer onlret "ONLRET" nil t)
 (:integer ofdel "OFDEL" nil t)
 (:integer ofill "OFILL" nil t)
 (:integer nldly "NLDLY" nil t)
 (:integer nl0 "NL0" nil t)
 (:integer nl1 "NL1" nil t)
 (:integer crdly "CRDLY" nil t)
 (:integer cr0 "CR0" nil t)
 (:integer cr1 "CR1" nil t)
 (:integer cr2 "CR2" nil t)
 (:integer cr3 "CR3" nil t)
 (:integer tabdly "TABDLY" nil t)
 (:integer tab0 "TAB0" nil t)
 (:integer tab1 "TAB1" nil t)
 (:integer tab2 "TAB2" nil t)
 (:integer tab3 "TAB3" nil t)
 (:integer bsdly "BSDLY" nil t)
 (:integer bs0 "BS0" nil t)
 (:integer bs1 "BS1" nil t)
 (:integer vtdly "VTDLY" nil t)
 (:integer vt0 "VT0" nil t)
 (:integer vt1 "VT1" nil t)
 (:integer ffdly "FFDLY" nil t)
 (:integer ff0 "FF0" nil t)
 (:integer ff1 "FF1" nil t)

 (:integer b0 "B0" nil t)
 (:integer b50 "B50" nil t)
 (:integer b75 "B75" nil t)
 (:integer b110 "B110" nil t)
 (:integer b134 "B134" nil t)
 (:integer b150 "B150" nil t)
 (:integer b200 "B200" nil t)
 (:integer b300 "B300" nil t)
 (:integer b600 "B600" nil t)
 (:integer b1200 "B1200" nil t)
 (:integer b1800 "B1800" nil t)
 (:integer b2400 "B2400" nil t)
 (:integer b4800 "B4800" nil t)
 (:integer b9600 "B9600" nil t)
 (:integer b19200 "B19200" nil t)
 (:integer b38400 "B38400" nil t)
 (:integer b57600 "B57600" nil t)
 (:integer b115200 "B115200" nil t)
 (:integer b230400 "B230400" nil t)

 (:integer csize "CSIZE" nil t)
 (:integer cs5 "CS5" nil t)
 (:integer cs6 "CS6" nil t)
 (:integer cs7 "CS7" nil t)
 (:integer cs8 "CS8" nil t)
 (:integer cstopb "CSTOPB" nil t)
 (:integer cread "CREAD" nil t)
 (:integer parenb "PARENB" nil t)
 (:integer parodd "PARODD" nil t)
 (:integer hupcl "HUPCL" nil t)
 (:integer clocal "CLOCAL" nil t)

 (:integer echo "ECHO" nil t)
 (:integer echoe "ECHOE" nil t)
 (:integer echok "ECHOK" nil t)
 (:integer echonl "ECHONL" nil t)
 (:integer icanon "ICANON" nil t)
 (:integer iexten "IEXTEN" nil t)
 (:integer isig "ISIG" nil t)
 (:integer noflsh "NOFLSH" nil t)
 (:integer tostop "TOSTOP" nil t)

 (:integer tcsanow "TCSANOW" nil t)
 (:integer tcsadrain "TCSADRAIN" nil t)
 (:integer tcsaflush "TCSAFLUSH" nil t)

 (:integer tciflush "TCIFLUSH" nil t)
 (:integer tcioflush "TCIOFLUSH" nil t)
 (:integer tcoflush "TCOFLUSH" nil t)
 (:integer tcioff "TCIOFF" nil t)
 (:integer tcion "TCION" nil t)
 (:integer tcooff "TCOOFF" nil t)
 (:integer tcoon "TCOON" nil t)

 ;; syslog -- does this exist at all on Windows?

 ;; SUSv3-standard openlog() facilities
 #-win32
 (:integer log-user "LOG_USER" "Default openlog() faclity." t)
 #-win32
 (:integer log-local0 "LOG_LOCAL0" "Locally-defined openlog() facility" t)
 #-win32
 (:integer log-local1 "LOG_LOCAL1" "Locally-defined openlog() facility" t)
 #-win32
 (:integer log-local2 "LOG_LOCAL2" "Locally-defined openlog() facility" t)
 #-win32
 (:integer log-local3 "LOG_LOCAL3" "Locally-defined openlog() facility" t)
 #-win32
 (:integer log-local4 "LOG_LOCAL4" "Locally-defined openlog() facility" t)
 #-win32
 (:integer log-local5 "LOG_LOCAL5" "Locally-defined openlog() facility" t)
 #-win32
 (:integer log-local6 "LOG_LOCAL6" "Locally-defined openlog() facility" t)
 #-win32
 (:integer log-local7 "LOG_LOCAL7" "Locally-defined openlog() facility" t)

 ;; Additional, non-standard openlog() facilities (most of which
 ;; probably won't be needed by Lisp programs, but here for
 ;; completeness).
 #-(or win32 sunos)
 (:integer
  log-authpriv "LOG_AUTHPRIV" "openlog() facility for authorization messages" t)
 #-win32
 (:integer
  log-cron "LOG_CRON" "openlog() facility for cron and at daemons" t)
 #-win32
 (:integer
  log-daemon "LOG_DAEMON" "openlog() facility for arbitrary daemons" t)
 #-(or win32 sunos)
 (:integer
  log-ftp "LOG_FTP" "openlog() facility for FTP daemons" t)
 #-win32
 (:integer
  log-kern "LOG_KERN" "openlog() facility for kernel messages" t)
 #-win32
 (:integer
  log-lpr "LOG_LPR" "openlog() facility for the printer subsystem" t)
 #-win32
 (:integer
  log-mail "LOG_MAIL" "openlog() facility for the mail subsystem" t)
 #-win32
 (:integer
  log-news "LOG_NEWS" "openlog() facility for the usenet subsystem" t)
 #-win32
 (:integer
  log-syslog "LOG_SYSLOG" "openlog() facility for the syslog subsystem" t)
 #-win32
 (:integer
  log-uucp "LOG_UUCP" "openlog() facility for the UUCP subsystem" t)

 ;; openlog() options
 #-win32
 (:integer
  log-pid "LOG_PID"
  "If supplied to openlog(), log the process ID with each message"
  t)
 #-win32
 (:integer
  log-cons "LOG_CONS"
  "If supplied to openlog(), log to the system console as well as logfiles"
  t)
 #-win32
 (:integer
  log-ndelay "LOG_NDELAY"
  "If supplied to openlog(), immediately open the syslog connection."
  t)
 #-win32
 (:integer
  log-odelay "LOG_ODELAY"
  "If supplied to openlog(), delay opening the syslog connection to the first syslog() call."
  t)
 #-win32
 (:integer
  log-nowait "LOG_NOWAIT"
  "If supplied to openlog(), do not wait for child processes created by calls to syslog()."
  t)
 ;; Not in SUSv3, but at least Glibc and BSD libc have this
 #-(or win32 sunos)
 (:integer
  log-perror "LOG_PERROR"
  "If supplied to openlog(), write log messages to the process's standard error descriptor in addition to the logging facility."
  t)

 ;; syslog() severity levels
 #-win32
 (:integer
  log-emerg "LOG_EMERG" "Log severity level denoting a panic." t)
 #-win32
 (:integer
  log-alert "LOG_ALERT" "Log severity level denoting a condition that should be corrected immediately." t)
 #-win32
 (:integer
  log-crit "LOG_CRIT" "Log severity level denoting critical conditions." t)
 #-win32
 (:integer
  log-err "LOG_ERR" "Log severity level denoting an error." t)
 #-win32
 (:integer
  log-warning "LOG_WARNING" "Log severity level denoting a warning." t)
 #-win32
 (:integer
  log-notice "LOG_NOTICE" "Log severity level denoting non-errors that may require special handling." t)
 #-win32
 (:integer
  log-info "LOG_INFO" "Log severity level denoting informational messages." t)
 #-win32
 (:integer
  log-debug "LOG_DEBUG" "Log severity level denoting debugging information ." t))
